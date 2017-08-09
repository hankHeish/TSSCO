
--First Taable, Measuring the Rank of Stock by Quarter
create table #TablePSR(�~�u nvarchar(50), �Ѳ��N�� nvarchar(50), ��~���J numeric(18, 4), MarketVal numeric(18, 4), PSR numeric(18, 4))
insert #TablePSR
select PSR.�~�u,
	   PSR.�Ѳ��N��,
	   PSR.[��~���J(�d)], 
	   PSR.MarketVal,
	   PSR.MarketVal / PSR.[��~���J(�d)] as PSR
from 
(
	SELECT FinStat.[�~�u]
		 , FinStat.[�Ѳ��N��]
		 , FinStat.[�Ѳ��W��]
		 , FinStat.[��~�Q�q(�d)] + FinStat.[��~����(�d)] + FinStat.[��~�O��(�d)] as [��~���J(�d)]
		 , MktVal.���L�� * Stock.[�ѥ�(�d)] as MarketVal
	FROM [Cmoney].[dbo].[�uIFRS�]��(�l�q��u)] as FinStat
	join
	(
		select Qtry.���
			, Qtry.�Ѳ��N��
			, Qtry.���L��
			--, Qtry.Qtr
			, CONCAT(YEAR(Qtry.���), Qtry.Qtr) as YYQQ
		from 
		(
			select RowRank.*, ROW_NUMBER() over (partition by �Ѳ��N��, YEAR(���), Qtr order by �Ѳ��N��, Qtr, ��� desc) as row_index
			from
			(
				select [���]
					, CONCAT('0', DATEPART(QUARTER, ���)) as Qtr
					,[�Ѳ��N��]
					,[���L��]
				from [Cmoney].[dbo].[�馬�L��Ʀ�]
				where LEN(�Ѳ��N��) = 4 and YEAR(���) between '2010' and '2017'
				--order by �Ѳ��N��, Qtr, ��� desc

			)as RowRank
		)as Qtry
		where Qtry.row_index = 1
	)as MktVal on(MktVal.YYQQ = FinStat.�~�u and MktVal.�Ѳ��N�� = FinStat.�Ѳ��N��)
	join
	(
		select  [�~�u], 
				[�Ѳ��N��], 
				[�Ѳ��W��], 
				[�ѥ�(�d)]
		from [Cmoney].[dbo].[�uIFRS�]��(�겣�t��)]
		where LEFT(�~�u, 4) between '2010' and '2017'
	)as Stock on (Stock.�~�u = FinStat.�~�u and Stock.�Ѳ��N�� = FinStat.�Ѳ��N��)
	where LEFT(FinStat.�~�u, 4) between '2010' and '2017'
)as PSR
where PSR.[��~���J(�d)] is not NULL and PSR.[��~���J(�d)] <> 0

create table #PSRGroup(�~�u nvarchar(50), �Ѳ��N�� nvarchar(50), PSR numeric(18, 4), PSRGroup numeric(18, 0))
insert #PSRGroup
select PSRGroup.�~�u, 
	   PSRGroup.�Ѳ��N��, 
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
			   rank() over(partition by �~�u order by PSR) as BottomRank, 
			   rank() over(partition by �~�u order by PSR desc) as TopRank
		from #TablePSR
	) as PSRRank
	where PSRRank.BottomRank <= 90 or PSRRank.TopRank <= 90
)as PSRGroup
order by PSRGroup.�~�u, PSRGroup.PSRGroup

--Table Daily Return to Calculate Stock Daily Return
create table #DailyReturn(��� datetime, StockID nvarchar(50), ���L�� numeric(18, 4), MMDD nvarchar(50), FinQutar nvarchar(50), PrevPrice numeric(18, 4), LogRet numeric(18, 4))
insert #DailyReturn
SELECT  Ret.���,
		Ret.�Ѳ��N�� as StockID, 
		--Ret.�Ѳ��W��, 
		Ret.���L��,
		Ret.MMDD,
		Ret.FinQutar,
		Ret.PrevPrice,
		LOG(Ret.���L��/Ret.PrevPrice) as LogRet
FROM
(
	SELECT *, 
			case
				when MMDD > '0515' and MMDD <= '0814' then CONCAT(YEAR(���), '01') 
				when MMDD > '0814' and MMDD <= '1114' then CONCAT(YEAR(���), '02') 
				when (MMDD > '1114' and MMDD <= '1231') then CONCAT(YEAR(���), '03') 
				when (MMDD > '0101' and MMDD <= '0331') then CONCAT(YEAR(���) - 1, '03') 
				when MMDD > '0331' and MMDD <= '0515' then CONCAT(YEAR(���) - 1, '04') 
			end as FinQutar,
			LAG(���L��)  over(partition by �Ѳ��N�� order by  ���) as PrevPrice
			--LOG(���L��/LAG(���L��, 1)) as ret
	from 
	(
		SELECT [���]
			,[�Ѳ��N��]
			,[�Ѳ��W��]
			,[���L��]
			, SUBSTRING(CONVERT(nvarchar, ���, 112), 5, 8) as MMDD
		FROM [Cmoney].[dbo].[�馬�L��Ʀ�]
		where YEAR(���) between '2010' and '2017'
			and �Ѳ��N�� between '1101' and '9962'
			and LEN(�Ѳ��N��) = 4
	)as PrcTable
)as Ret

create table #PSR_ES_Table(��� datetime, �~�u nvarchar(50), DailyRet numeric(18, 4), PSRGroup numeric(18, 0), IndexRet numeric(18, 4), ES numeric(18, 4))
insert #PSR_ES_Table
select *, 
	   GroupRS.DailyRet - GroupRS.IndexRet as RS
from 
(
	select DailyRet.���, 
		   DailyRet.�~�u, 
		   DailyRet.DailyRet, 
		   DailyRet.PSRGroup, 
		   SUM(DailyRet.DailyRet) over(partition by DailyRet.��� order by DailyRet.���) / 6 as IndexRet
	from 
	(
		select *, 
			   SUM(AvgGroupRet_Daily.LogRet) over(partition by AvgGroupRet_Daily.���, AvgGroupRet_Daily.PSRGroup
					 order by AvgGroupRet_Daily.���, AvgGroupRet_Daily.PSRGroup) / 30 as DailyRet, 
			   ROW_NUMBER() over(partition by AvgGroupRet_Daily.���, AvgGroupRet_Daily.PSRGroup
					 order by AvgGroupRet_Daily.���, AvgGroupRet_Daily.PSRGroup) as RowIndex
		from 
		(
			select PSR.�~�u ,
				   PSR.�Ѳ��N��, 
				   PSR.PSR, 
				   PSR.PSRGroup, 
				   Ret.LogRet,
				   Ret.���
			from #PSRGroup as PSR
			join 
			(
				select *
				from #DailyReturn
			) as Ret on (PSR.�~�u = Ret.FinQutar and PSR.�Ѳ��N�� = Ret.StockID)
		)as AvgGroupRet_Daily
	)as DailyRet
	where DailyRet.RowIndex = 1
)as GroupRS
order by GroupRS.���, GroupRS.PSRGroup

--select * from #PSR_ES_Table

select SUMMARY.�~�u, 
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
		   AVG(IndexRet) over(partition by PQUD.�~�u, PQUD.PSRGroup order by PQUD.�~�u, PQUD.PSRGroup) as AvgIndexRet,
		   ROW_NUMBER() over(partition by PQUD.�~�u, PQUD.PSRGroup order by PQUD.�~�u, PQUD.PSRGroup) as RowIndex
	from
	(
		select *, 
			   SUM(EPEQ.ProWin) over(partition by PSRGroup, �~�u order by PSRGroup, �~�u) as WinSum,
			   SUM(EPEQ.ProLoss) over(partition by PSRGroup, �~�u order by PSRGroup, �~�u) as LossSum
		from 
		(
			select *,
				   SUM(PQ.CountP) over (partition by PSRGroup, �~�u order by PSRGroup, �~�u) as TotalP,
				   SUM(PQ.CountQ) over (partition by PSRGroup, �~�u order by PSRGroup, �~�u) as TotalQ, 
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
order by SUMMARY.�~�u

/*
select * from #PSRGroup
select * from #DailyReturn
select * from #PSR_ES_Table
*/

drop table #TablePSR
drop table #PSRGroup
drop table #DailyReturn
drop table #PSR_ES_Table