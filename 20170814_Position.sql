declare @t date 
set @t=(select max(日期) from  cmoney.dbo.自營商進出週報表)--每周的周末


declare @out table(日期 datetime,台指 dec(20,5),電子 dec(20,5),金融 dec(20,5),個股期 dec(20,5),海外期 dec(20,5),東證期 dec(20,5))

declare @期貨部位 table (Txdate datetime,Account Nvarchar(5),凱基Account Nvarchar(5),Stockid Nvarchar (20),[國/內外] Nvarchar(4),Volume int,LastPrice decimal(10,2),CallPut varchar(1),Strike_Price decimal(20,2))
declare @JPY dec(10,7) set  @JPY =(  select 匯率 from   cmoney.[dbo].[台新銀行每日匯率] where 日期=@t and 幣別='JPY')
declare @USD dec(10,7) set  @USD =(  select 匯率 from   cmoney.[dbo].[台新銀行每日匯率] where 日期=@t and 幣別='USD')
declare @HKD dec(10,7) set  @HKD =(  select 匯率 from   cmoney.[dbo].[台新銀行每日匯率] where 日期=@t and 幣別='HKD')
declare @EUR dec(10,7) set  @EUR =(  select 匯率 from   cmoney.[dbo].[台新銀行每日匯率] where 日期=@t and 幣別='EUR')

declare @期貨契約結算價 table (Txdate datetime,Stockid Nvarchar (20),結算價 decimal(7,2),callput varchar(1),strikeprice decimal(20,2))
insert @期貨契約結算價
select distinct 匯入日期,FTR_ID1+FTR_MTH1,結算價1,CallPut1,Strike_Price1 from pl.dbo.期貨_未平倉資料
where 匯入日期=@T 

--select * from @期貨契約結算價 where Stockid='TXO201709'



declare @期貨部位契約乘數總檔1 table (Txdate datetime,Stockid Nvarchar (20),契約乘數 int,Tag varchar(20))
insert @期貨部位契約乘數總檔1
select distinct 日期,代號,契約乘數, 
case when left(代號,2) in ('MX', 'TX') then '台指期'
     when left(代號,2) in ('EX') then '電子期' 
	 when left(代號,2) in ('FX') then '金融期' 
	 when left(代號,2) in ('TJ') then '東證期' 
	 when left(代號,3) in ('TXO') then '台指選' 
	 else  '個股期'  end  as Tag
from MarketData.dbo.每日個股期貨契約乘數 where 日期=@T

declare @海外期貨契約乘數 table (Txdate datetime,Stockid Nvarchar (20),契約乘數 int,Tag varchar(20))
insert @海外期貨契約乘數
select @t,行情代號,契約乘數,'海外期' from DBMain.dbo.海外期貨代號對照表


insert @期貨部位契約乘數總檔1
select @t,'MX1',50,'台指期'
insert @期貨部位契約乘數總檔1
select @t,'MX2',50,'台指期'
insert @期貨部位契約乘數總檔1
select @t,'MX3',50,'台指期'
insert @期貨部位契約乘數總檔1
select @t,'MX4',50,'台指期'
insert @期貨部位契約乘數總檔1
select @t,'OLF',100,'個股期'
insert @期貨部位契約乘數總檔1
select @t,'ONF',100,'個股期'
insert @期貨部位契約乘數總檔1
select @t,'OMF',100,'個股期'
insert @期貨部位契約乘數總檔1
select @t,'EXF',4000,'電子期'

insert @海外期貨契約乘數
select @t,'CN2',1,'海外期'
insert @海外期貨契約乘數
select @t,'CL2',1000,'海外期'
insert @海外期貨契約乘數
select @t,'GC2',100,'海外期'
insert @海外期貨契約乘數
select @t,'TOP',1000,'海外期' ---TOPIXM201703
insert @海外期貨契約乘數
select @t,'ES2',50,'海外期' 
insert @海外期貨契約乘數
select @t,'FDX',5,'海外期' --FDXM
insert @海外期貨契約乘數
select @t,'CC2',10,'海外期' ---可可期貨
insert @海外期貨契約乘數
select @t,'YM2',5,'海外期' 
insert @海外期貨契約乘數
select @t,'SI2',50,'海外期' 
insert @海外期貨契約乘數
select @t,'KC2',375,'海外期' 
insert @海外期貨契約乘數
select @t,'PL2',50,'海外期' ---白銀期貨
insert @海外期貨契約乘數
select @t,'SIN',2,'海外期' ---白銀期貨

/*
insert  @期貨部位
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.判斷類別(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '國外' else '國內' end [國/內外],
Volume,LastPrice from PL.dbo.Inventory_衍商自營 as a
join DBMain.dbo.帳號交易類別 as b on a.Acc=b.FuAcc00 and b.交易類別='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 
 
insert  @期貨部位
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.判斷類別(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '國外' else '國內' end [國/內外],
Volume,LastPrice from PL.dbo.Inventory_期交 as a
join DBMain.dbo.帳號交易類別 as b on a.Acc=b.FuAcc00 and b.交易類別='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 

insert  @期貨部位
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.判斷類別(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '國外' else '國內' end [國/內外],
Volume,LastPrice from PL.dbo.Inventory_策交 as a
join DBMain.dbo.帳號交易類別 as b on a.Acc=b.FuAcc00 and b.交易類別='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 

insert  @期貨部位
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.判斷類別(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '國外' else '國內' end [國/內外],
Volume,LastPrice from PL.dbo.Inventory_債券 as a
join DBMain.dbo.帳號交易類別 as b on a.Acc=b.FuAcc00 and b.交易類別='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 

insert  @期貨部位
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.判斷類別(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '國外' else '國內' end [國/內外],
Volume,LastPrice from PL.dbo.Inventory_自營二組 as a
join DBMain.dbo.帳號交易類別 as b on a.Acc=b.FuAcc00 and b.交易類別='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 
*/


insert  @期貨部位
select 匯入日期,Acc00,FuAcc00,FTR_ID1+FTR_MTH1 as 代號,case when DBMain.dbo.判斷類別(FTR_ID1+FTR_MTH1) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '國外' else '國內' end [國/內外], 
sum(case when BuySell1='S' then -1*未平倉口數1 else 未平倉口數1 end )as 口數,結算價1,CallPut1,Strike_Price1
  from pl.dbo.期貨_未平倉資料 as a
  join DBMain.dbo.帳號交易類別 as b
  on a.交易代號=b.FuAcc00
where 匯入日期=@t and b.交易類別='G2'--and 交易代號='5102'
group by 匯入日期,FTR_ID1+FTR_MTH1,結算價1,FuAcc00,b.Acc00,CallPut1,Strike_Price1
order by FuAcc00,FTR_ID1+FTR_MTH1


--select * from  @期貨部位


/*  上面已經計算過
insert  @期貨部位
select @t,StockID,Volume,收盤價 from PL.dbo.Inventory_衍商ETF
where TxDate=@t and Acc like ('____') and Volume <>0 
*/

--select * from  @期貨部位


declare @未沖銷總表 table (Txdate datetime,Account Nvarchar(5),凱基Account Nvarchar(5),Stockid Nvarchar (20),[國/內外] Nvarchar(4),Volume int,契約乘數 int,LastPrice decimal(10,2),未沖銷金額 decimal(20,4),Tag varchar(20),callput varchar(1),strikeprice decimal(20,2))
insert @未沖銷總表
select a.Txdate,Account,凱基Account,a.Stockid,a.[國/內外],SUM(a.volume) as 總量,b.契約乘數,isnull(c.結算價,LastPrice),abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) as 未沖銷金額,Tag,a.callput,strikeprice
from @期貨部位 as a
join @期貨部位契約乘數總檔1 as b  on b.Txdate=a.Txdate and b.Stockid=left(a.Stockid,3)
left join @期貨契約結算價 as c on c.Stockid=a.Stockid and a.CallPut=c.callput and a.Strike_Price=c.strikeprice
group by a.Txdate,Account,凱基Account,a.Stockid,a.[國/內外],c.結算價,b.契約乘數,Tag,LastPrice,a.callput,strikeprice
order by Txdate,Stockid

--select * from  @期貨部位
--select * from @期貨部位契約乘數總檔1 where Stockid='TXO'
--select * from @期貨契約結算價
--select * from @未沖銷總表


insert @未沖銷總表
select a.Txdate,Account,凱基Account,a.Stockid,a.[國/內外],SUM(a.volume) as 總量,b.契約乘數,isnull(c.結算價,LastPrice),
case when a.Stockid like 'CN%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'GC%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'CL%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'YM%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'HHI%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @HKD
when a.Stockid like 'HSI%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @HKD
when a.Stockid like 'NK%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @JPY
when a.Stockid like 'JT%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @JPY
when a.Stockid like 'TOPIX%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @JPY
when a.Stockid like 'ES%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'FDXM%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @EUR
when a.Stockid like 'CC%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'SI%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'KC%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'PL%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
when a.Stockid like 'SIN%' then abs(SUM(a.volume))*b.契約乘數*isnull(c.結算價,LastPrice) * @USD
end as 未沖銷金額,Tag,a.callput,strikeprice
from @期貨部位 as a
join @海外期貨契約乘數 as b  on b.Txdate=a.Txdate and b.Stockid=left(a.Stockid,3)
left join @期貨契約結算價 as c on c.Stockid=a.Stockid and a.CallPut=c.callput
group by a.Txdate,Account,凱基Account,a.Stockid,a.[國/內外],c.結算價,b.契約乘數,Tag,LastPrice,a.CallPut,strikeprice
order by Txdate,Stockid

--select * from @期貨部位
--select * from @期貨契約結算價 
--select * from @未沖銷總表


select Txdate as 日期,Account,凱基Account,[國/內外],
       SUBSTRING(Stockid,1,CHARINDEX('2', [Stockid])-1) as 代號,
	   right(Stockid,6) as 月份,
	   Volume,契約乘數,
	   LastPrice as 結算價,
case when DBMain.dbo.判斷類別(stockid) in('TOPIX','NK') then @JPY
     when DBMain.dbo.判斷類別(stockid) in('A50','YM','CL','GC','IN','TW','ES','CC','SI','KC','PL','SIN') then @USD
	 when DBMain.dbo.判斷類別(stockid) in('HSI','HHI','MHI') then @HKD
	 when DBMain.dbo.判斷類別(stockid) in('FDXM') then @EUR
	 else 1 end as 匯率
--,case when Volume<0 then 0 else 未沖銷金額 end as 長部位
--,case when Volume>0 then 0 else 未沖銷金額 end as 短部位
from @未沖銷總表
