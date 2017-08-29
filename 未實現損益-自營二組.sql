declare @sDate datetime 
set @sDate =
(
	select Top 1 Tradingdate
	from [DBMain].[dbo].[Tradingdate]
	where MONTH(Tradingdate) = (case
									when  MONTH(CONVERT(datetime, GETDATE(), 105)) = 1 then 12
									else MONTH(CONVERT(datetime, GETDATE(), 105)) - 1
								end )
		and YEAR(Tradingdate) = (case 
									when MONTH(CONVERT(datetime, GETDATE(), 105)) = 1 then YEAR(CONVERT(datetime, GETDATE(), 105)) - 1
									else YEAR(CONVERT(datetime, GETDATE(), 105))
								 end )
	order by Tradingdate desc 
)

create table #AcctPortf(Acc nvarchar(50), Portfolio nvarchar(50))
insert #AcctPortf
SELECT distinct(Acc)
	  , Portfolio
FROM [DBMain].[dbo].[DefaultPortfolio_自營二組]
where BDate <= @sDate and EDate >= @sDate

--declare @Date1 datetime, @Date2 datetime
--set @Date1 = '2017-06-30'
--set @Date2 = '20170630'
select * 
	  ,FUT_PL + Stk_PL + Loan_PL as CumPL
from
(
	select 交易員代碼　as StockID
		  ,交易代號 as FutID
		  ,LoanID
		  ,case 
		       when Fut.Portfolio is not NULL then Fut.Portfolio
			   when Stk.Portfolio is not NULL then Stk.Portfolio
			   when Loan.Portfolio is not NULL then Loan.Portfolio
		   end as Portfolio
		  ,case
			   when Cum_Fut_PL is NULL then 0
			   else Cum_Fut_PL
		   end as FUT_PL
		  ,case
			   when Cum_Stk_PL is NULL then 0
			   else Cum_Stk_PL
		   end as Stk_PL
		  ,case
			   when Loan_CumPL is NULL then 0
			   else Loan_CumPL
		   end as Loan_PL
	from
	(
		--Stock CumPL
		SELECT distinct([交易員代碼])
			  ,Portfolio
			  ,SUM([今日未實現損益]) over(partition by 交易員代碼 order by 交易員代碼) as Cum_Stk_PL
		FROM [PL].[dbo].[400現股庫存檔wkstkx] as Stock
		join #AcctPortf as Portfolio on (Stock.交易員代碼 = Portfolio.Acc)
		--WHERE  庫存日期 = @Date2
		WHERE  庫存日期 = @sDate
	)as Stk
	full join 
	(
		--Futures CumPL
		SELECT distinct([交易代號])
			  ,Portfolio
			  ,SUM([未平倉損益]) over(partition by 交易代號 order by 交易代號) as Cum_Fut_PL
		FROM [PL].[dbo].[期貨_未平倉資料] as Acc_Fut
		join #AcctPortf as Portfolio on (Acc_Fut.交易代號 = Portfolio.Acc)
		--where 匯入日期 = @Date1
		where 匯入日期 = @sDate
	)as Fut on (Stk.Portfolio = Fut.Portfolio)
	full join
	(
		--Loan PL
		select distinct(交易員代碼) as LoanID
			  ,Portfolio
			  ,SUM(今日未實現損益) over (partition by 交易員代碼 order by 交易員代碼) as Loan_CumPL
		from [PL].[dbo].[400券賣庫存損益檔wklnkx] as LoanPos
		join #AcctPortf as Portfolio on (Portfolio.Acc = LoanPos.交易員代碼)
		where 庫存日期 = @sDate
	)as Loan on (Loan.LoanID = Stk.交易員代碼)
)as TotalPL
order by TotalPL.StockID

drop table #AcctPortf