/****** SSMS 中 SelectTopNRows 命令的指令碼  ******/

create table #PEGTable(日期 datetime, YYQQ nvarchar(50), 股票代號 nvarchar(50), 本益比 numeric(18, 4), YOY numeric(18, 4), PEG numeric(18, 4))
insert #PEGTable
select PETable.日期, 
	   PETable.YYQQ,
	   PETable.股票代號, 
	   PETable.本益比, 
	   GrowthRate.YOY, 
	   PETable.本益比 / GrowthRate.YOY as PEG
from
(
	select *, 
		   ROW_NUMBER() over(partition by 股票代號, YYQQ order by 股票代號, 日期) as rowindex
	from 
	(
		SELECT [日期]
			  ,[股票代號]
			  ,[股票名稱]
			  ,[收盤價]
			  ,[本益比]
			  , CONCAT(YEAR(日期), CONCAT('0', DATEPART(QUARTER, 日期))) as YYQQ
		FROM [Cmoney].[dbo].[日收盤表排行]
		where YEAR(日期) between '2010' and '2017'
			  and LEN(股票代號) = 4
			  and 股票代號 between '1101' and '9962'
	)as RowIndex
)as PETable
join
(
	select *,
		   (([營業收入淨額累計(千)] / LastOptInc) - 1) * 100 as YOY, 
		    CONCAT(LEFT(年季, 4), QQ) as YYQQ
	from
	(
		select *, 
				LAG([營業收入淨額累計(千)]) over(partition by QQ, 股票代號 order by 股票代號, 年季) as LastOptInc
		from
		(
			SELECT [年季]
					,[股票代號]
					,[股票名稱]
					,[營業收入淨額累計(千)]
					, RIGHT(年季, 2) as QQ
			FROM [Cmoney].[dbo].[季IFRS財報(損益累計)]
			where LEFT(年季, 4) between '2010' and '2017'
		)as AugTable
	)as YOY
	where YOY.LastOptInc <> 0
) as GrowthRate on (PETable.股票代號 = GrowthRate.股票代號 and PETable.YYQQ = GrowthRate.YYQQ)
where PETable.rowindex = 1
order by PETable.股票代號, PETable.日期

--select * from #PEGTable



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

create table #TempRS_PFG(日期 datetime, YYQQ nvarchar(50), LogRet numeric(18, 4), PEGRank numeric(18, 0), AvgDailyRet numeric(18, 4), AvgDailyIdxRet numeric(18, 4), RS numeric(18, 4))
insert #TempRS_PFG
select PEGStock.日期,
	   PEGStock.YYQQ,
	   PEGStock.LogRet, 
	   PEGStock.PEGRank, 
	   PEGStock.AvgDailyRet, 
	   PEGStock.AvgDailyIdxRet, 
	   PEGStock.AvgDailyRet - PEGStock.AvgDailyIdxRet as RSRet
from 
(
	select Ret.日期,
		   Ret.StockID,
		   Ret.LogRet, 
		   PEGRank.PEGRank, 
		   PEGRank.YYQQ,
		   SUM(LogRet) over(partition by Ret.日期, PEGRank.PEGRank order by Ret.日期, PEGRank.PEGRank) / 30 as AvgDailyRet,
		   SUM(LogRet) over(partition by Ret.日期 order by Ret.日期) / 180 as AvgDailyIdxRet,
		   ROW_NUMBER() over(partition by Ret.日期, PEGRank.PEGRank order by Ret.日期, PEGRank.PEGRank) as RowIndex
	from #DailyReturn as Ret 
	join
	(
		select *,
				case 
					when TopPEG <= 30 then 1
					when TopPEG > 30 and TopPEG <= 60 then 2
					when TopPEG > 60 and TopPEG <= 90 then 3

					when BottomPEG <= 30 then 6
					when BottomPEG > 30 and BottomPEG <= 60 then 5
					when BottomPEG > 60 and BottomPEG <= 90 then 4
				end as PEGRank, 
				SUBSTRING(CONVERT(nvarchar, 日期, 112), 5, 8) as MMDD
		from 
		(
			select *, 
					RANK() over(partition by YYQQ order by PEG desc) as TopPEG,
					RANK() over(partition by YYQQ order by PEG ) as BottomPEG
			from #PEGTable
			where PEG is not NULL
		)as PEGGroup
		where TopPEG <= 90 or BottomPEG <= 90
	)as PEGRank on (Ret.FinQutar = PEGRank.YYQQ and Ret.StockID = PEGRank.股票代號)
) as PEGStock
where PEGStock.RowIndex = 1
order by PEGStock.日期, PEGStock.PEGRank

--select * from #TempRS_PFG

create table #PQUDtable(年季 nvarchar(50), PEGRank numeric(18, 0), AvgPU numeric(18, 4), AvgQD numeric(18, 4), PUQD numeric(18, 4), AvgIdxRet numeric(18, 4))
insert #PQUDtable
select QuarTable.YYQQ, 
	   QuarTable.PEGRank, 
	   QuarTable.AvgPU, 
	   QuarTable.AvgQD,
	   QuarTable.PUQD,
	   QuarTable.AvgIdxRet
from 
(
	select *,
		   SUM(PQUDtable.PU) over(partition by YYQQ, PEGRank order by YYQQ, PEGRank)/CountP as AvgPU, 
		   SUM(PQUDtable.QD) over(partition by YYQQ, PEGRank order by YYQQ, PEGRank)/CountQ as AvgQD,
		   SUM(PQUDtable.PU + PQUDtable.QD) over(partition by YYQQ, PEGRank order by YYQQ, PEGRank)/(CountP + CountQ) as PUQD, 
		   AVG(PQUDtable.LogRet) over(partition by YYQQ, PEGRank order by YYQQ, PEGRank) as AvgIdxRet,
		   ROW_NUMBER() over (partition by YYQQ, PEGRank order by YYQQ, PEGRank) as RowIndex
	from 
	(
		select *, 
			   AvgDailyRet * P as PU, 
			   AvgDailyRet * Q as QD
		from 
		(
			select *, 
				   SUM(PQRate.P) over(partition by YYQQ, PEGRank order by YYQQ, PEGRank) as CountP, 
				   SUM(PQRate.Q) over(partition by YYQQ, PEGRank order by YYQQ, PEGRank) as CountQ
			from
			(
				select *, 
					   case 
							when RS > 0 then 1
							else 0
					   end as P, 
					   case 
							when RS < 0 then 1
							else 0
					   end as Q
				from #TempRS_PFG
			)as PQRate
		)as Cal_PUQD
	)as PQUDtable
)as QuarTable
where QuarTable.RowIndex = 1
order by QuarTable.PEGRank, QuarTable.日期

select * 
from #PQUDtable as Base
pivot
(
	年季, PEGRaank, AvgPU, AvgQD, AvgIdxRet
)


drop table #PEGTable
drop table #DailyReturn
drop table #TempRS_PFG
drop table #PQUDtable