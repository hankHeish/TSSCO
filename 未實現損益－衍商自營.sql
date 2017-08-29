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
FROM [DBMain].[dbo].[DefaultPortfolio_�l�Ӧ���]
where BDate <= @sDate and EDate >= @sDate

--select * from #AcctPortf
--declare @Date1 datetime, @Date2 datetime
--set @Date1 = '2017-06-30'
--set @Date2 = '20170630'
SELECT *
	  ,Stk_PL + Fut_PL + Loan_PL as CumPL
FROM
(
	SELECT StockID
		  ,FutID
		  ,LoanID
		  ,case 
		       when Fut_Acct.Portfolio is not NULL then Fut_Acct.Portfolio
			   when Stk_Acct.Portfolio is not NULL then Stk_Acct.Portfolio
			   when Loan.Portfolio is not NULL then Loan.Portfolio
		   end as Portfolio
		  ,case 
			   when nReal_Stk_CumPL is NULL then 0
			   else nReal_Stk_CumPL
		   end as Stk_PL
		  ,case 
			   when nReal_Fut_CmuPL is NULL then 0
			   else nReal_Fut_CmuPL
		   end as Fut_PL
		  ,case 
			   when Loan_CumPL is NULL then 0
			   else Loan_CumPL
		   end as Loan_PL
	FROM
	(
		--Stock PL
		select distinct(������N�X) as StockID
				,Portfolio
				,SUM(���饼��{�l�q) over(partition by ������N�X order by ������N�X) as nReal_Stk_CumPL
		from [PL].[dbo].[400�{�Ѯw�s��wkstkx] as Stock
		join #AcctPortf as Portfolio on(Stock.������N�X = Portfolio.Acc)
		--where �w�s��� = @Date2
		where �w�s��� = @sDate
	)as Stk_Acct
	full join
	(
		--Futures PL
		select distinct(����N��) as FutID
				,Portfolio
				,SUM(�����ܷl�q) over(partition by ����N�� order by ����N��) as nReal_Fut_CmuPL
		from [PL].[dbo].[���f_�����ܸ��] as Acct_Fut
		join #AcctPortf as Acct on(Acct_Fut.����N�� = Acct.Acc)
		--where �פJ��� = @Date1
		where �פJ��� = @sDate
	)as Fut_Acct on (Stk_Acct.Portfolio = Fut_Acct.Portfolio)
	full join
	(
		--Loan PL
		select distinct(������N�X) as LoanID
			  ,Portfolio
			  ,SUM(���饼��{�l�q) over (partition by ������N�X order by ������N�X) as Loan_CumPL
		from [PL].[dbo].[400���w�s�l�q��wklnkx] as LoanPos
		join #AcctPortf as Portfolio on (Portfolio.Acc = LoanPos.������N�X)
		where �w�s��� = @sDate
	)as Loan on (Loan.LoanID = Stk_Acct.StockID)
)as TotalPL
order by TotalPL.StockID


drop table #AcctPortf