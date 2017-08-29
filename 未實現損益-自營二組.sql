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
FROM [DBMain].[dbo].[DefaultPortfolio_����G��]
where BDate <= @sDate and EDate >= @sDate

--declare @Date1 datetime, @Date2 datetime
--set @Date1 = '2017-06-30'
--set @Date2 = '20170630'
select * 
	  ,FUT_PL + Stk_PL + Loan_PL as CumPL
from
(
	select ������N�X�@as StockID
		  ,����N�� as FutID
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
		SELECT distinct([������N�X])
			  ,Portfolio
			  ,SUM([���饼��{�l�q]) over(partition by ������N�X order by ������N�X) as Cum_Stk_PL
		FROM [PL].[dbo].[400�{�Ѯw�s��wkstkx] as Stock
		join #AcctPortf as Portfolio on (Stock.������N�X = Portfolio.Acc)
		--WHERE  �w�s��� = @Date2
		WHERE  �w�s��� = @sDate
	)as Stk
	full join 
	(
		--Futures CumPL
		SELECT distinct([����N��])
			  ,Portfolio
			  ,SUM([�����ܷl�q]) over(partition by ����N�� order by ����N��) as Cum_Fut_PL
		FROM [PL].[dbo].[���f_�����ܸ��] as Acc_Fut
		join #AcctPortf as Portfolio on (Acc_Fut.����N�� = Portfolio.Acc)
		--where �פJ��� = @Date1
		where �פJ��� = @sDate
	)as Fut on (Stk.Portfolio = Fut.Portfolio)
	full join
	(
		--Loan PL
		select distinct(������N�X) as LoanID
			  ,Portfolio
			  ,SUM(���饼��{�l�q) over (partition by ������N�X order by ������N�X) as Loan_CumPL
		from [PL].[dbo].[400���w�s�l�q��wklnkx] as LoanPos
		join #AcctPortf as Portfolio on (Portfolio.Acc = LoanPos.������N�X)
		where �w�s��� = @sDate
	)as Loan on (Loan.LoanID = Stk.������N�X)
)as TotalPL
order by TotalPL.StockID

drop table #AcctPortf