select BackTest.TxDate,
	   AVG(BackTest.Ret) as AvgRet
from 
(
	select Ret.TxDate, 
		   day(Ret.TxDate) as sDate,
		   Ret.TimeTag, 
		   Ret.profit, 
		   Ret.CumPL, 
		   Ret.Pos, 
		   Ret.CumPos, 
		   Ret.profit / Ret.Margin as Ret
	from 
	(
		select Margin.TxDate, 
		   Margin.TimeTag, 
		   Margin.profit, 
		   Margin.CumPL, 
		   Margin.Pos, 
		   Margin.CumPos, 
		   case 
				when Margin.profit + Margin.Margin < Margin.Maintain THEN ABS(Margin.Pos) +  Margin.Margin
				when Margin.profit + Margin.Margin > Margin.Maintain THEN Margin.Margin
		   end as Margin
		from
		(
			select CumPL.TxDate, 
			   CumPL.TimeTag, 
			   CumPL.profit, 
			   CumPL.CumPL, 
			   CumPL.Pos, 
			   sum(CumPL.Pos) over(order by CumPL.TxDate, CumPL.TimeTag rows between unbounded preceding and current row)
					as CumPos, 
			   abs(CumPL.Pos) * 63000 + abs(CumPL.Pos) * 3 / 2 * 58000 as Margin, 
			   abs(CumPL.Pos) * 52000 + abs(CumPL.Pos) * 3 / 2 * 41000 as Maintain
			from 
			(
				select Pos.TxDate, 
					   Pos.TimeTag, 
					   Pos.Signal, 
					   Pos.profit, 
					   sum(Pos.profit) over(order by Pos.TxDate, Pos.TimeTag rows between unbounded preceding and current row)
							as CumPL, 
					   case 
							when Pos.Signal = 1 then -2
							when Pos.Signal = 2 then 2
					   end as Pos
				from 
				(
					select PL.TxDate, 
						   PL.TimeTag, 
						   PL.Signal, 
						   case
								when PL.Signal = 1 then 2 * PL.TickPrice_EXF * 4000 - 3 * PL.TickPrice_FXF * 1000
								when PL.Signal = 2 then -2 * PL.TickPrice_EXF * 4000 + 3 * PL.TickPrice_FXF * 1000
						   end as profit
					from 
					(
						select Signal.TxDate, 
								Signal.TimeTag, 
								Signal.TickPrice_EXF近月 as TickPrice_EXF,
								LAG(Signal.TickPrice_EXF近月) over(order by Signal.TxDate, Signal.TimeTag) as Lag_TickPrice_EXF, 
								Signal.TickPrice_FXF近月 as TickPrice_FXF,
								LAG(Signal.TickPrice_FXF近月) over(order by Signal.TxDate, Signal.TimeTag) as Lag_TickPrice_FXF, 
								case 
									when Signal.A_Value < Signal.min_35_A_Value then 1	--Short E, Long B
									when Signal.A_Value > Signal.max_35_A_Value then 2	--Long E, Short B
									else 0
								end as Signal
						from 
						(
							select temp.TxDate
									, temp.TimeTag
									, temp.TickPrice_EXF近月
									, temp.TickPrice_FXF近月
									, temp.A_Value
									, min(temp.A_Value) over(order by temp.TxDate, temp.TimeTag rows between 35 preceding and 1 preceding)
										as min_35_A_Value
									, max(temp.A_Value) over(order by temp.TxDate, temp.TimeTag rows between 35 preceding and 1 preceding)
										as max_35_A_Value
							from 
							(
								select a.TxDate,a.TimeTag
   										,a.TickPrice_EXF近月
										,a.TickPrice_FXF近月
										, Dvr.[M2300 電子類指數]
										, Dvr.[M2800 金融保險類指數]
										, 2 * a.TickPrice_EXF近月 * Dvr.[M2300 電子類指數] - 3 * a.TickPrice_FXF近月 * Dvr.[M2800 金融保險類指數]
											as A_Value
								from intraday.[dbo].[指數期貨每分鐘] a
									join dbmain.dbo.TimeTag_FU b on left(b.TimeTag,4)=a.Timetag and b.ser between 1 and 300
									join (select * from
											(
												SELECT distinct([公司代碼])
													,[年月日]
													,[指數基值]
												FROM [MarketData].[dbo].[TEJ指數成分股股數]
												where 公司代碼 like 'M2800%' or 公司代碼 like 'M2300%'
											) as Base
											pivot 
											(
												sum(指數基值) for 公司代碼 in ([M2300 電子類指數], [M2800 金融保險類指數])
											)as Trans
										) as Dvr on Dvr.年月日 = a.TxDate

								where a.TxDate>='2017/7/1'
				
							)as temp
						)as Signal
					)as PL
				) as Pos
			)as CumPL
		)as Margin
	)as Ret
	where Ret.profit is not NULL
)as BackTest
group by BackTest.TxDate, BackTest.sDate
order by BackTest.TxDate




