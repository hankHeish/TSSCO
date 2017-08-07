/****** SSMS �� SelectTopNRows �R�O�����O�X  ******/
  
--Create table #template(��� datetime, �~�u nvarchar(50), �Ѳ��N�� nvarchar(50), LogRet numeric(18, 4), PortGroup numeric(18, 4))
--create table #���˰϶�(�~�u nvarchar(50),BDate datetime,EDate datetime)


Create table #template(��� datetime, �~�u nvarchar(50), �Ѳ��N�� nvarchar(50), LogRet numeric(18, 4), PortGroup numeric(18, 0))
insert #template
select StockSelection.���, 
		StockSelection.�~�u, 
		StockSelection.StockID,
		StockSelection.LogRet, 
		StockSelection.PortGroup
from 
(
	select *,
			case 
				when Portf.Rank_Top <= 90 then 1
				when Portf.Rank_Bottom <= 90 then 2
			end as Trd,
			case 
				when Portf.Rank_Top <= 30 then 1
				when 30 < Portf.Rank_Top and Portf.Rank_Top <= 60 then 2
				when 30 < Portf.Rank_Top and Portf.Rank_Top <= 90 then 3

				when Portf.Rank_Bottom <= 30 then 6
				when 30 < Portf.Rank_Bottom and Portf.Rank_Bottom <= 60 then 5
				when 60 < Portf.Rank_Bottom and Portf.Rank_Bottom <= 90 then 4
			end as PortGroup
	from
	(
		select Table1.�~�u, 
				Table1.�Ѳ��N��,
				Table1.EntLvg, 
				rank() over(partition by Table1.�~�u order by Table1.EntLvg desc) as Rank_Top, 
				rank() over(partition by Table1.�~�u order by Table1.EntLvg) as Rank_Bottom
		from 
		(
			select * 
					, INDEX1.EV/INDEX1.EBITDA as EntLvg
			from 
			(
				SELECT ALSheet.[�~�u],
						--,SUBSTRING(ALSheet.�~�u, 5, 2) as Qtr
						ALSheet.[�Ѳ��N��],
						ALSheet.[�Ѳ��W��],
						--,ALSheet.[�t���`�p(�d)]
						--,ALSheet.[�ѥ�(�d)]  
						--,ALSheet.[�{���ά���{��(�d)]
						--, CloPrc.�Ѳ��N��
						--, CloPrc.���L��
						ALSheet.[�ѥ�(�d)] * CloPrc.���L�� - ALSheet.[�t���`�p(�d)] + ALSheet.[�{���ά���{��(�d)] as EV,
						--, Debt.�~�u
						--, Debt.�Ѳ��N��
						Debt.EBITDA
						--, CloPrc.YYQQ
						--, CloPrc.���
				FROM [Cmoney].[dbo].[�uIFRS�]��(�겣�t��)] as ALSheet
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
							where len(�Ѳ��N��) = 4 and YEAR(���) between '2010' and '2017'
							--order by �Ѳ��N��, Qtr, ��� desc

						)as RowRank
					)as Qtry
					where Qtry.row_index = 1
					--order by Qtry.�Ѳ��N��, Qtry.���

				) as CloPrc on (CloPrc.�Ѳ��N�� = ALSheet.�Ѳ��N�� and CloPrc.YYQQ = ALSheet.�~�u)
				join
				(
					SELECT [�~�u]
						,[�Ѳ��N��]
						,[�Ѳ��W��]
						,[��~�Q�q(�d)] as EBITDA

					FROM [Cmoney].[dbo].[�uIFRS�]��(�l�q��u)]
					where left(�~�u, 4) between '2010' and '2017'
				) as Debt on (Debt.�Ѳ��N�� = ALSheet.�Ѳ��N�� and Debt.�~�u = ALSheet.�~�u)
				where SUBSTRING(ALSheet.�~�u, 1, 4) between '2010' and '2017'
			)as INDEX1
			where INDEX1.EBITDA is not NULL and INDEX1.EV is not NULL and INDEX1.EBITDA <> 0

		)as Table1
	) as Portf
	join 
	(
		SELECT  Ret.���,
				Ret.�Ѳ��N�� as StockID, 
				Ret.�Ѳ��W��, 
				Ret.�}�L��,
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
						when (MMDD > '0101' and MMDD <= '0515') then CONCAT(YEAR(���) - 1, '03') 
						when MMDD > '0331' and MMDD <= '0515' then CONCAT(YEAR(���) - 1, '04') 
					end as FinQutar,
					LAG(���L��)  over(partition by �Ѳ��N�� order by  ���) as PrevPrice
					--LOG(���L��/LAG(���L��, 1)) as ret
			from 
			(
				SELECT [���]
						,[�Ѳ��N��]
						,[�Ѳ��W��]
						,[�}�L��]
						,[���L��]
						, SUBSTRING(CONVERT(nvarchar, ���, 112), 5, 8) as MMDD
					FROM [Cmoney].[dbo].[�馬�L��Ʀ�]
					where YEAR(���) between '2010' and '2017'
						and �Ѳ��N�� between '1101' and '9962'
						and LEN(�Ѳ��N��) = 4
			)as PrcTable
		)as Ret
		--order by  Ret.�Ѳ��N��, Ret.���
	)as Price on (Portf.�~�u = Price.FinQutar and Portf.�Ѳ��N�� = Price.StockID)
	where Portf.Rank_Top <= 90 or Portf.Rank_Bottom <= 90
) as StockSelection

--select * from #template

select *, 
	   ESTable.DailyLogRet - ESTable.AvgIndexRet as ES
from
(
	select *, 
		   SUM(AvgTableIndex.DailyLogRet) over(partition by ��� order by ���) / 6 as AvgIndexRet
	from
	(
		select TableIndex.���, 
				TableIndex.�~�u, 
				TableIndex.DailyLogRet / 30 as DailyLogRet, 
				TableIndex.PortGroup
		from 
		(
			select * , 
					SUM(LogRet) over(partition by ���, PortGroup order by ���, PortGroup) as DailyLogRet, 
					ROW_NUMBER() over (partition by ���, PortGroup order by ���, PortGroup) as RowIndex
			from #template
		)as TableIndex
		where TableIndex.RowIndex = 1
	)as AvgTableIndex
)as ESTable
order by ESTable.���, ESTable.PortGroup







drop table #template