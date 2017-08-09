

Create table #template(日期 datetime, 年季 nvarchar(50), 股票代號 nvarchar(50), LogRet numeric(18, 4), PortGroup numeric(18, 0))
insert #template
select StockSelection.日期, 
		StockSelection.年季, 
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
		select Table1.年季, 
				Table1.股票代號,
				Table1.EntLvg, 
				rank() over(partition by Table1.年季 order by Table1.EntLvg desc) as Rank_Top, 
				rank() over(partition by Table1.年季 order by Table1.EntLvg) as Rank_Bottom
		from 
		(
			select * 
					, INDEX1.EV/INDEX1.EBITDA as EntLvg
			from 
			(
				SELECT ALSheet.[年季],
						--,SUBSTRING(ALSheet.年季, 5, 2) as Qtr
						ALSheet.[股票代號],
						ALSheet.[股票名稱],
						--,ALSheet.[負債總計(千)]
						--,ALSheet.[股本(千)]  
						--,ALSheet.[現金及約當現金(千)]
						--, CloPrc.股票代號
						--, CloPrc.收盤價
						ALSheet.[股本(千)] * CloPrc.收盤價 + ALSheet.[負債總計(千)] - ALSheet.[現金及約當現金(千)] as EV,
						--, Debt.年季
						--, Debt.股票代號
						Debt.EBITDA
						--, CloPrc.YYQQ
						--, CloPrc.日期
				FROM [Cmoney].[dbo].[季IFRS財報(資產負債)] as ALSheet
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
							where len(股票代號) = 4 and YEAR(日期) between '2010' and '2017'
							--order by 股票代號, Qtr, 日期 desc

						)as RowRank
					)as Qtry
					where Qtry.row_index = 1
					--order by Qtry.股票代號, Qtry.日期

				) as CloPrc on (CloPrc.股票代號 = ALSheet.股票代號 and CloPrc.YYQQ = ALSheet.年季)
				join
				(
					SELECT [年季]
						,[股票代號]
						,[股票名稱]
						,[營業利益(千)] as EBITDA

					FROM [Cmoney].[dbo].[季IFRS財報(損益單季)]
					where left(年季, 4) between '2010' and '2017'
				) as Debt on (Debt.股票代號 = ALSheet.股票代號 and Debt.年季 = ALSheet.年季)
				where SUBSTRING(ALSheet.年季, 1, 4) between '2010' and '2017'
			)as INDEX1
			where INDEX1.EBITDA is not NULL and INDEX1.EV is not NULL and INDEX1.EBITDA <> 0

		)as Table1
	) as Portf
	join 
	(
		SELECT  Ret.日期,
				Ret.股票代號 as StockID, 
				Ret.股票名稱, 
				Ret.開盤價,
				Ret.收盤價,
				Ret.MMDD,
				Ret.FinQutar,
				Ret.PrevPrice,
				LOG(Ret.收盤價/Ret.PrevPrice) as LogRet
		FROM
		(
			SELECT *, 
				   case
					   when (MMDD > '0515' and MMDD <= '0814') then CONCAT(YEAR(日期), '01') 
					   when (MMDD > '0814' and MMDD <= '1114') then CONCAT(YEAR(日期), '02') 
					   when (MMDD > '1114' and MMDD <= '1231') then CONCAT(YEAR(日期), '03') 
					   when (MMDD > '0101' and MMDD <= '0331') then CONCAT(YEAR(日期) - 1, '03') 
					   when (MMDD > '0331' and MMDD <= '0515') then CONCAT(YEAR(日期) - 1, '04') 
				   end as FinQutar,
				   LAG(收盤價)  over(partition by 股票代號 order by  日期) as PrevPrice
				--LOG(收盤價/LAG(收盤價, 1)) as ret
			from 
			(
				SELECT [日期]
					,[股票代號]
					,[股票名稱]
					,[開盤價]
					,[收盤價]
					, SUBSTRING(CONVERT(nvarchar, 日期, 112), 5, 8) as MMDD
				FROM [Cmoney].[dbo].[日收盤表排行]
				where YEAR(日期) between '2010' and '2017'
					and 股票代號 between '1101' and '9962'
					and LEN(股票代號) = 4
			)as PrcTable
		)as Ret
		--order by  Ret.股票代號, Ret.日期
	)as Price on (Portf.年季 = Price.FinQutar and Portf.股票代號 = Price.StockID)
	where Portf.Rank_Top <= 90 or Portf.Rank_Bottom <= 90
) as StockSelection

--select * from #template

select * 
from #template
order by 日期

create table #ESTable(日期 datetime, 年季 nvarchar(50), AvgDailyLogRet numeric(18, 4), PortGroup numeric(18, 0), AvgIdxLogRet numeric(18, 4), ES numeric(18, 4), P numeric(18, 0), Q numeric(18, 0))
insert #ESTable
select *, 
	   case 
			when ES > 0 then 1
			else 0
	   end as P,
	   case 
			when ES < 0 then 1
			else 0
	   end as Q
from
(
	select *, 
		   AvgDailyLogRet - AvgIdxLogRet_byDate as ES
	from
	(
		select  TableIndex.日期, 
				TableIndex.年季, 
				TableIndex.AvgDailyLogRet, 
				TableIndex.PortGroup,
				AVG(AvgDailyLogRet) over(partition by 日期 order by 日期) as AvgIdxLogRet_byDate
		from 
		(
			select * , 
					AVG(LogRet) over(partition by 日期, PortGroup order by 年季, PortGroup) as AvgDailyLogRet, 
					ROW_NUMBER() over (partition by 日期, PortGroup order by 年季, PortGroup) as RowIndex
			from #template
		)as TableIndex
		where TableIndex.RowIndex = 1
	)as PortfES
)as ESTable
order by ESTable.日期, ESTable.PortGroup

--select * from #ESTable

create table #PQUDTable(年季 nvarchar(50), PortGroup numeric(18, 0), AvgIdxRet numeric(18, 4), AvgPU numeric(18, 4), AvgQD numeric(18, 4), AvgPQUD numeric(18, 4))
insert #PQUDTable
select PUQDtable.年季, 
	   PUQDtable.PortGroup, 
	   PUQDtable.AvgIdxLogRet,
	   PUQDtable.AvgPU, 
	   PUQDtable.AvgQD, 
	   PUQDtable.AvgPUQD
from
(
	select *,
		   AVG(PU) over(partition by 年季, PortGroup order by 年季, PortGroup) as AvgPU,
		   AVG(QD) over(partition by 年季, PortGroup order by 年季, PortGroup) as AvgQD,
		   AVG(PU + QD) over(partition by 年季, PortGroup order by 年季, PortGroup) as AvgPUQD,
		   ROW_NUMBER() over(partition by 年季, PortGroup order by 年季, PortGroup) as RowIndex
	from
	(
		select *,
			   AvgDailyLogRet * P as PU, 
			   AvgDailyLogRet * Q as QD
		from #ESTable
	)as Cal_PUQD
)as PUQDtable
where PUQDtable.RowIndex = 1
order by PUQDtable.年季


select * from #PQUDTable

drop table #template
drop table #ESTable
drop table #PQUDTable