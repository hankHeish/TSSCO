
--First Taable, Measuring the Rank of Stock by Quarter
create table #TablePSR(年季 nvarchar(50), 股票代號 nvarchar(50), 營業收入 numeric(18, 4), MarketVal numeric(18, 4), PSR numeric(18, 4))
insert #TablePSR
select PSR.年季,
	   PSR.股票代號,
	   PSR.[營業收入(千)], 
	   PSR.MarketVal,
	   PSR.MarketVal / PSR.[營業收入(千)] as PSR
from 
(
	SELECT FinStat.[年季]
		 , FinStat.[股票代號]
		 , FinStat.[股票名稱]
		 , FinStat.[營業利益(千)] + FinStat.[營業成本(千)] + FinStat.[營業費用(千)] as [營業收入(千)]
		 , MktVal.收盤價 * Stock.[股本(千)] as MarketVal
	FROM [Cmoney].[dbo].[季IFRS財報(損益單季)] as FinStat
	join
	(
		select Qtry.日期
			, Qtry.股票代號
			, Qtry.收盤價
			--, Qtry.Qtr
			, CONCAT(YEAR(Qtry.日期), Qtry.Qtr) as YYQQ
		from 
		(
			select RowRank.*, ROW_NUMBER() over (partition by 股票代號, YEAR(日期), Qtr order by 股票代號, Qtr, 日期 desc) as row_index
			from
			(
				select [日期]
					, CONCAT('0', DATEPART(QUARTER, 日期)) as Qtr
					,[股票代號]
					,[收盤價]
				from [Cmoney].[dbo].[日收盤表排行]
				where LEN(股票代號) = 4 and YEAR(日期) between '2010' and '2017'
				--order by 股票代號, Qtr, 日期 desc

			)as RowRank
		)as Qtry
		where Qtry.row_index = 1
	)as MktVal on(MktVal.YYQQ = FinStat.年季 and MktVal.股票代號 = FinStat.股票代號)
	join
	(
		select  [年季], 
				[股票代號], 
				[股票名稱], 
				[股本(千)]
		from [Cmoney].[dbo].[季IFRS財報(資產負債)]
		where LEFT(年季, 4) between '2010' and '2017'
	)as Stock on (Stock.年季 = FinStat.年季 and Stock.股票代號 = FinStat.股票代號)
	where LEFT(FinStat.年季, 4) between '2010' and '2017'
)as PSR
where PSR.[營業收入(千)] is not NULL and PSR.[營業收入(千)] <> 0

create table #PSRGroup(年季 nvarchar(50), 股票代號 nvarchar(50), PSR numeric(18, 4), PSRGroup numeric(18, 0))
insert #PSRGroup
select PSRGroup.年季, 
	   PSRGroup.股票代號, 
	   PSRGroup.PSR, 
	   PSRGroup.PSRGroup
from
(
	select *, 
			case
				when PSRRank.TopRank <= 30 then 1
				when PSRRank.TopRank > 30 and PSRRank.TopRank <= 60 then 2
				when PSRRank.TopRank > 30 and PSRRank.TopRank <= 90 then 3

				when PSRRank.BottomRank <= 30 then 4
				when PSRRank.BottomRank > 30 and PSRRank.BottomRank <= 60 then 5
				when PSRRank.BottomRank > 30 and PSRRank.BottomRank <= 90 then 6
			end as PSRGroup
	from 
	(
		select * , 
			   rank() over(partition by 年季 order by PSR) as BottomRank, 
			   rank() over(partition by 年季 order by PSR desc) as TopRank
		from #TablePSR
	) as PSRRank
	where PSRRank.BottomRank <= 90 or PSRRank.TopRank <= 90
)as PSRGroup
order by PSRGroup.年季, PSRGroup.PSRGroup

--Table Daily Return to Calculate Stock Daily Return
create table #DailyReturn(日期 datetime, StockID nvarchar(50), 收盤價 numeric(18, 4), MMDD nvarchar(50), FinQutar nvarchar(50), PrevPrice numeric(18, 4), LogRet numeric(18, 4))
insert #DailyReturn
SELECT  Ret.日期,
		Ret.股票代號 as StockID, 
		--Ret.股票名稱, 
		Ret.收盤價,
		Ret.MMDD,
		Ret.FinQutar,
		Ret.PrevPrice,
		LOG(Ret.收盤價/Ret.PrevPrice) as LogRet
FROM
(
	SELECT *, 
			case
				when MMDD > '0515' and MMDD <= '0814' then CONCAT(YEAR(日期), '01') 
				when MMDD > '0814' and MMDD <= '1114' then CONCAT(YEAR(日期), '02') 
				when (MMDD > '1114' and MMDD <= '1231') then CONCAT(YEAR(日期), '03') 
				when (MMDD > '0101' and MMDD <= '0331') then CONCAT(YEAR(日期) - 1, '03') 
				when MMDD > '0331' and MMDD <= '0515' then CONCAT(YEAR(日期) - 1, '04') 
			end as FinQutar,
			LAG(收盤價)  over(partition by 股票代號 order by  日期) as PrevPrice
			--LOG(收盤價/LAG(收盤價, 1)) as ret
	from 
	(
		SELECT [日期]
			,[股票代號]
			,[股票名稱]
			,[收盤價]
			, SUBSTRING(CONVERT(nvarchar, 日期, 112), 5, 8) as MMDD
		FROM [Cmoney].[dbo].[日收盤表排行]
		where YEAR(日期) between '2010' and '2017'
			and 股票代號 between '1101' and '9962'
			and LEN(股票代號) = 4
	)as PrcTable
)as Ret

create table #PSR_ES_Table(日期 datetime, 年季 nvarchar(50), DailyRet numeric(18, 4), PSRGroup numeric(18, 0), IndexRet numeric(18, 4), ES numeric(18, 4))
insert #PSR_ES_Table
select *, 
	   GroupRS.DailyRet - GroupRS.IndexRet as RS
from 
(
	select DailyRet.日期, 
		   DailyRet.年季, 
		   DailyRet.DailyRet, 
		   DailyRet.PSRGroup, 
		   SUM(DailyRet.DailyRet) over(partition by DailyRet.日期 order by DailyRet.日期) / 6 as IndexRet
	from 
	(
		select *, 
			   SUM(AvgGroupRet_Daily.LogRet) over(partition by AvgGroupRet_Daily.日期, AvgGroupRet_Daily.PSRGroup
					 order by AvgGroupRet_Daily.日期, AvgGroupRet_Daily.PSRGroup) / 30 as DailyRet, 
			   ROW_NUMBER() over(partition by AvgGroupRet_Daily.日期, AvgGroupRet_Daily.PSRGroup
					 order by AvgGroupRet_Daily.日期, AvgGroupRet_Daily.PSRGroup) as RowIndex
		from 
		(
			select PSR.年季 ,
				   PSR.股票代號, 
				   PSR.PSR, 
				   PSR.PSRGroup, 
				   Ret.LogRet,
				   Ret.日期
			from #PSRGroup as PSR
			join 
			(
				select *
				from #DailyReturn
			) as Ret on (PSR.年季 = Ret.FinQutar and PSR.股票代號 = Ret.StockID)
		)as AvgGroupRet_Daily
	)as DailyRet
	where DailyRet.RowIndex = 1
)as GroupRS
order by GroupRS.日期, GroupRS.PSRGroup

--select * from #PSR_ES_Table

select SUMMARY.年季, 
	   SUMMARY.PSRGroup,
	   SUMMARY.PU, 
	   SUMMARY.QD, 
	   SUMMARY.PUQD,
	   SUMMARY.AvgIndexRet
from 
(
	select *,
		   PQUD.WinSum / PQUD.TotalP as PU, 
		   PQUD.LossSum / PQUD.TotalQ as QD, 
		   (PQUD.WinSum + PQUD.LossSum) / (PQUD.TotalP + PQUD.TotalQ) as PUQD, 
		   AVG(IndexRet) over(partition by PQUD.年季, PQUD.PSRGroup order by PQUD.年季, PQUD.PSRGroup) as AvgIndexRet,
		   ROW_NUMBER() over(partition by PQUD.年季, PQUD.PSRGroup order by PQUD.年季, PQUD.PSRGroup) as RowIndex
	from
	(
		select *, 
			   SUM(EPEQ.ProWin) over(partition by PSRGroup, 年季 order by PSRGroup, 年季) as WinSum,
			   SUM(EPEQ.ProLoss) over(partition by PSRGroup, 年季 order by PSRGroup, 年季) as LossSum
		from 
		(
			select *,
				   SUM(PQ.CountP) over (partition by PSRGroup, 年季 order by PSRGroup, 年季) as TotalP,
				   SUM(PQ.CountQ) over (partition by PSRGroup, 年季 order by PSRGroup, 年季) as TotalQ, 
				   PQ.DailyRet * PQ.CountP as ProWin, 
				   PQ.DailyRet * PQ.CountQ as ProLoss
			from
			(
				select *, 
					   case 
							when ES > 0 then 1
							when ES <= 0 then 0
					   end as CountP, 
					   case 
							when ES < 0 then 1
							when ES >= 0 then 0
					   end as CountQ
				from #PSR_ES_Table
			)as PQ
		)as EPEQ
	)as PQUD
)SUMMARY
where SUMMARY.RowIndex = 1
order by SUMMARY.年季

/*
select * from #PSRGroup
select * from #DailyReturn
select * from #PSR_ES_Table
*/

drop table #TablePSR
drop table #PSRGroup
drop table #DailyReturn
drop table #PSR_ES_Table