declare @t date 
set @t=(select max(���) from  cmoney.dbo.����Ӷi�X�g����)--�C�P���P��


declare @out table(��� datetime,�x�� dec(20,5),�q�l dec(20,5),���� dec(20,5),�ӪѴ� dec(20,5),���~�� dec(20,5),�F�Ҵ� dec(20,5))

declare @���f���� table (Txdate datetime,Account Nvarchar(5),�Ͱ�Account Nvarchar(5),Stockid Nvarchar (20),[��/���~] Nvarchar(4),Volume int,LastPrice decimal(10,2),CallPut varchar(1),Strike_Price decimal(20,2))
declare @JPY dec(10,7) set  @JPY =(  select �ײv from   cmoney.[dbo].[�x�s�Ȧ�C��ײv] where ���=@t and ���O='JPY')
declare @USD dec(10,7) set  @USD =(  select �ײv from   cmoney.[dbo].[�x�s�Ȧ�C��ײv] where ���=@t and ���O='USD')
declare @HKD dec(10,7) set  @HKD =(  select �ײv from   cmoney.[dbo].[�x�s�Ȧ�C��ײv] where ���=@t and ���O='HKD')
declare @EUR dec(10,7) set  @EUR =(  select �ײv from   cmoney.[dbo].[�x�s�Ȧ�C��ײv] where ���=@t and ���O='EUR')

declare @���f��������� table (Txdate datetime,Stockid Nvarchar (20),����� decimal(7,2),callput varchar(1),strikeprice decimal(20,2))
insert @���f���������
select distinct �פJ���,FTR_ID1+FTR_MTH1,�����1,CallPut1,Strike_Price1 from pl.dbo.���f_�����ܸ��
where �פJ���=@T 

--select * from @���f��������� where Stockid='TXO201709'



declare @���f���쫴�������`��1 table (Txdate datetime,Stockid Nvarchar (20),�������� int,Tag varchar(20))
insert @���f���쫴�������`��1
select distinct ���,�N��,��������, 
case when left(�N��,2) in ('MX', 'TX') then '�x����'
     when left(�N��,2) in ('EX') then '�q�l��' 
	 when left(�N��,2) in ('FX') then '���Ĵ�' 
	 when left(�N��,2) in ('TJ') then '�F�Ҵ�' 
	 when left(�N��,3) in ('TXO') then '�x����' 
	 else  '�ӪѴ�'  end  as Tag
from MarketData.dbo.�C��ӪѴ��f�������� where ���=@T

declare @���~���f�������� table (Txdate datetime,Stockid Nvarchar (20),�������� int,Tag varchar(20))
insert @���~���f��������
select @t,�污�N��,��������,'���~��' from DBMain.dbo.���~���f�N����Ӫ�


insert @���f���쫴�������`��1
select @t,'MX1',50,'�x����'
insert @���f���쫴�������`��1
select @t,'MX2',50,'�x����'
insert @���f���쫴�������`��1
select @t,'MX3',50,'�x����'
insert @���f���쫴�������`��1
select @t,'MX4',50,'�x����'
insert @���f���쫴�������`��1
select @t,'OLF',100,'�ӪѴ�'
insert @���f���쫴�������`��1
select @t,'ONF',100,'�ӪѴ�'
insert @���f���쫴�������`��1
select @t,'OMF',100,'�ӪѴ�'
insert @���f���쫴�������`��1
select @t,'EXF',4000,'�q�l��'

insert @���~���f��������
select @t,'CN2',1,'���~��'
insert @���~���f��������
select @t,'CL2',1000,'���~��'
insert @���~���f��������
select @t,'GC2',100,'���~��'
insert @���~���f��������
select @t,'TOP',1000,'���~��' ---TOPIXM201703
insert @���~���f��������
select @t,'ES2',50,'���~��' 
insert @���~���f��������
select @t,'FDX',5,'���~��' --FDXM
insert @���~���f��������
select @t,'CC2',10,'���~��' ---�i�i���f
insert @���~���f��������
select @t,'YM2',5,'���~��' 
insert @���~���f��������
select @t,'SI2',50,'���~��' 
insert @���~���f��������
select @t,'KC2',375,'���~��' 
insert @���~���f��������
select @t,'PL2',50,'���~��' ---�ջȴ��f
insert @���~���f��������
select @t,'SIN',2,'���~��' ---�ջȴ��f

/*
insert  @���f����
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.�P�_���O(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '��~' else '�ꤺ' end [��/���~],
Volume,LastPrice from PL.dbo.Inventory_�l�Ӧ��� as a
join DBMain.dbo.�b��������O as b on a.Acc=b.FuAcc00 and b.������O='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 
 
insert  @���f����
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.�P�_���O(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '��~' else '�ꤺ' end [��/���~],
Volume,LastPrice from PL.dbo.Inventory_���� as a
join DBMain.dbo.�b��������O as b on a.Acc=b.FuAcc00 and b.������O='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 

insert  @���f����
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.�P�_���O(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '��~' else '�ꤺ' end [��/���~],
Volume,LastPrice from PL.dbo.Inventory_���� as a
join DBMain.dbo.�b��������O as b on a.Acc=b.FuAcc00 and b.������O='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 

insert  @���f����
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.�P�_���O(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '��~' else '�ꤺ' end [��/���~],
Volume,LastPrice from PL.dbo.Inventory_�Ũ� as a
join DBMain.dbo.�b��������O as b on a.Acc=b.FuAcc00 and b.������O='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 

insert  @���f����
select @t,Acc00,FuAcc00,StockID,case when DBMain.dbo.�P�_���O(stockid) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '��~' else '�ꤺ' end [��/���~],
Volume,LastPrice from PL.dbo.Inventory_����G�� as a
join DBMain.dbo.�b��������O as b on a.Acc=b.FuAcc00 and b.������O='G2'
where TxDate=@t and Acc like ('____') and Volume <>0 
*/


insert  @���f����
select �פJ���,Acc00,FuAcc00,FTR_ID1+FTR_MTH1 as �N��,case when DBMain.dbo.�P�_���O(FTR_ID1+FTR_MTH1) in('A50','YM','CL','GC','HSI','HHI','MHI','NK','ES','FDXM','GC','KC','SI','PL','SIN') then '��~' else '�ꤺ' end [��/���~], 
sum(case when BuySell1='S' then -1*�����ܤf��1 else �����ܤf��1 end )as �f��,�����1,CallPut1,Strike_Price1
  from pl.dbo.���f_�����ܸ�� as a
  join DBMain.dbo.�b��������O as b
  on a.����N��=b.FuAcc00
where �פJ���=@t and b.������O='G2'--and ����N��='5102'
group by �פJ���,FTR_ID1+FTR_MTH1,�����1,FuAcc00,b.Acc00,CallPut1,Strike_Price1
order by FuAcc00,FTR_ID1+FTR_MTH1


--select * from  @���f����


/*  �W���w�g�p��L
insert  @���f����
select @t,StockID,Volume,���L�� from PL.dbo.Inventory_�l��ETF
where TxDate=@t and Acc like ('____') and Volume <>0 
*/

--select * from  @���f����


declare @���R�P�`�� table (Txdate datetime,Account Nvarchar(5),�Ͱ�Account Nvarchar(5),Stockid Nvarchar (20),[��/���~] Nvarchar(4),Volume int,�������� int,LastPrice decimal(10,2),���R�P���B decimal(20,4),Tag varchar(20),callput varchar(1),strikeprice decimal(20,2))
insert @���R�P�`��
select a.Txdate,Account,�Ͱ�Account,a.Stockid,a.[��/���~],SUM(a.volume) as �`�q,b.��������,isnull(c.�����,LastPrice),abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) as ���R�P���B,Tag,a.callput,strikeprice
from @���f���� as a
join @���f���쫴�������`��1 as b  on b.Txdate=a.Txdate and b.Stockid=left(a.Stockid,3)
left join @���f��������� as c on c.Stockid=a.Stockid and a.CallPut=c.callput and a.Strike_Price=c.strikeprice
group by a.Txdate,Account,�Ͱ�Account,a.Stockid,a.[��/���~],c.�����,b.��������,Tag,LastPrice,a.callput,strikeprice
order by Txdate,Stockid

--select * from  @���f����
--select * from @���f���쫴�������`��1 where Stockid='TXO'
--select * from @���f���������
--select * from @���R�P�`��


insert @���R�P�`��
select a.Txdate,Account,�Ͱ�Account,a.Stockid,a.[��/���~],SUM(a.volume) as �`�q,b.��������,isnull(c.�����,LastPrice),
case when a.Stockid like 'CN%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'GC%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'CL%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'YM%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'HHI%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @HKD
when a.Stockid like 'HSI%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @HKD
when a.Stockid like 'NK%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @JPY
when a.Stockid like 'JT%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @JPY
when a.Stockid like 'TOPIX%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @JPY
when a.Stockid like 'ES%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'FDXM%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @EUR
when a.Stockid like 'CC%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'SI%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'KC%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'PL%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
when a.Stockid like 'SIN%' then abs(SUM(a.volume))*b.��������*isnull(c.�����,LastPrice) * @USD
end as ���R�P���B,Tag,a.callput,strikeprice
from @���f���� as a
join @���~���f�������� as b  on b.Txdate=a.Txdate and b.Stockid=left(a.Stockid,3)
left join @���f��������� as c on c.Stockid=a.Stockid and a.CallPut=c.callput
group by a.Txdate,Account,�Ͱ�Account,a.Stockid,a.[��/���~],c.�����,b.��������,Tag,LastPrice,a.CallPut,strikeprice
order by Txdate,Stockid

--select * from @���f����
--select * from @���f��������� 
--select * from @���R�P�`��


select Txdate as ���,Account,�Ͱ�Account,[��/���~],
       SUBSTRING(Stockid,1,CHARINDEX('2', [Stockid])-1) as �N��,
	   right(Stockid,6) as ���,
	   Volume,��������,
	   LastPrice as �����,
case when DBMain.dbo.�P�_���O(stockid) in('TOPIX','NK') then @JPY
     when DBMain.dbo.�P�_���O(stockid) in('A50','YM','CL','GC','IN','TW','ES','CC','SI','KC','PL','SIN') then @USD
	 when DBMain.dbo.�P�_���O(stockid) in('HSI','HHI','MHI') then @HKD
	 when DBMain.dbo.�P�_���O(stockid) in('FDXM') then @EUR
	 else 1 end as �ײv
--,case when Volume<0 then 0 else ���R�P���B end as ������
--,case when Volume>0 then 0 else ���R�P���B end as �u����
from @���R�P�`��