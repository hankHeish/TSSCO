
declare @t datetime set @t=(select max(���) from cmoney.dbo.�v�Ұ򥻸�ƪ�C��) 
declare @y datetime set @y=dbmain.dbo.tradingdateadd(-1,@t)
declare @��Ƥ�� datetime set @��Ƥ��='2017/1/1'
--(select max(ResetDate) from [DBMain].[dbo].����겣�洫CBAS_Credit�ݳ��쭭�B)


declare @�򥻸�� table(�Ѳ��N�� nvarchar(50),�Ѳ��W�� nvarchar(50),�W���W�d nvarchar(50),[�Τ@�s��] nvarchar(50))
insert @�򥻸��
select �Ѳ��N��,�Ѳ��W��,�W���W�d,case when �Ѳ��N��='4991' then '53008500' else �Τ@�s�� end
from cmoney.[dbo].[�W���d���q�򥻸��]
insert @�򥻸��
select �N��,�W��,�W���W�d,[����νs]
from cmoney.[dbo].[ETF�򥻸��]
insert @�򥻸��
select �N��,�W��,3,[�Τ@�s��]
from cmoney.[dbo].[���d�򥻸��] where �~��=(select max(�~��) from cmoney.[dbo].[���d�򥻸��])
insert @�򥻸��
select �N��,�W��,4,[�Τ@�s��]
from cmoney.[dbo].[���}�o��򥻸��] where �~��=(select max(�~��) from cmoney.[dbo].[���}�o��򥻸��])


insert @�򥻸��
select '000'+a.�N��,a.�W��,5,a.[�Τ@�s��]
from cmoney.[dbo].[�Ȧ�����q�򥻸��] a 
left join @�򥻸�� b on b.�Τ@�s��=a.�Τ@�s��
where a.�~��=(select max(�~��) from cmoney.[dbo].[�Ȧ�����q�򥻸��])
and a.�Τ@�s�� is not null


declare @CB table(�N�� nvarchar(50),�W�� nvarchar(50),�ഫ�Ъ��N�� nvarchar(50),�Ũ��O�Ȧ�N�� nvarchar(50))
insert @CB
select b.�i��ťN�X,b.�i��ŦW��,b.�ഫ�Ъ�,case when a.�Ũ��O�Ȧ�N��='000081' then '000018' else a.�Ũ��O�Ȧ�N�� end
from cb.dbo.CBData b
left join cb.[dbo].[CB��O�Ȧ�N��]() a on b.�i��ťN�X=a.�N��
where b.TxDate=@t

--select *from �@@CB where �Ũ��O�Ȧ�N��='000018'


declare @���īH�� table(�«H��ID nvarchar(50),�H������ nvarchar(50))
insert @���īH��
select a.�«H��ID,a.�H������
from [CB].[dbo].[���īH��] a
join (select �«H��ID,max(��Ƥ��) as ��Ƥ�� from [CB].[dbo].[���īH��] where �H������<>'-'  group by �«H��ID) b on b.�«H��ID=a.�«H��ID and b.��Ƥ��=a.��Ƥ��


declare @CB�H�� table(�N�� nvarchar(50),�W�� nvarchar(50),�Ъ��N�� nvarchar(50),�H��_�o�椽�q nvarchar(50),�H��_��O�Ȧ� nvarchar(50),�H�� nvarchar(50),�H���B�פ��q nvarchar(50),[��O���] decimal(20,6))
insert @CB�H��
select distinct a.�N��,a.�W��,left(a.�N��,4)
,isnull(cast(left(c1.�H������,1) as int),9)
 as �H��_�o�椽�q
,isnull(cast(left(c2.�H������,1) as int),9)
 as �H��_��O�Ȧ�
,case when isnull(cast(left(c1.�H������,1) as int),9)<isnull(cast(left(c2.�H������,1) as int),9) then isnull(cast(left(c1.�H������,1) as int),9)
      else isnull(cast(left(c2.�H������,1) as int),9) end
 as �H��
,case when isnull(cast(left(c2.�H������,1) as int),9)<isnull(cast(left(c1.�H������,1) as int),9) then b2.�Ѳ��N��
      else left(a.�N��,4) end
 as �H���B�פ��q
,isnull(g.[��O���],1) as [��O���]
from @CB a
left join @�򥻸�� b1 on b1.�Ѳ��N��=left(a.�N��,4)
left join @�򥻸�� b2 on b2.�Ѳ��N��=a.�Ũ��O�Ȧ�N��
left join cb.[dbo].[�i��ž�O���] g on g.[�N��]=a.�N�� and g.[�Ũ��O�Ȧ�N��]=a.�Ũ��O�Ȧ�N�� and @t between g.bdate and g.edate
left join @���īH�� c1 on c1.�«H��ID=b1.�Τ@�s�� 
left join @���īH�� c2 on c2.�«H��ID=b2.�Τ@�s��
--order by a.�N��


declare @CB�H��group table(�N�� nvarchar(50),�W�� nvarchar(50),�H���B�פ��q nvarchar(50),�H������ nvarchar(50),[��O���] decimal(20,6))
insert @CB�H��group
select  �N��,�W��,�H���B�פ��q
,case when cast(min(�H��) as nvarchar(50)) in ('6','7','8','9') then '6+' 
	  else cast(min(�H��) as nvarchar(50)) end
,sum([��O���])
from @CB�H��
where �H�� is not null and �H���B�פ��q<>'000018'
group by �N��,�W��,�H���B�פ��q


--select * from @CB�H��group order by �N��


declare @�H�����ŭ��Btemp table(�H�� nvarchar(50),���쭱�B decimal(20,4))
declare @�H�����ŭ��B table(�H�� nvarchar(50),���쭱�B decimal(20,4))
/*
insert @�H�����ŭ��B
select case when d.�H������ in('6','7') then '6+' else d.�H������ end
,sum(a.�Ѽ�*100*d.��O���)/1000000.
 as MV
from PL.[dbo].[�Ũ�L�a����] a
join cb.dbo.CBData b on b.TxDate=@t and b.�i��ťN�X=a.�N��
left join @CB�H��group d on d.�N��=a.�N��
where a.TxDate=(select max(Txdate) from PL.[dbo].[�Ũ�L�a����] where a.Tag='CBAS')
group by case when d.�H������ in('6','7') then '6+' else d.�H������ end
*/
insert @�H�����ŭ��Btemp 
select case when d.�H������ in('6','7') then '6+' else d.�H������ end
,sum(a.ASW_PRINCIPAL*d.��O���)/1000000.
 as MV
from dbmain.dbo.CBAS_ASW a
join cb.dbo.CBData b on b.TxDate=a.TxDate and b.�i��ťN�X=a.ASW_CB
join cmoney.[dbo].[�W���d���q�򥻸��] c on c.�Ѳ��N��=b.�ഫ�Ъ�
left join @CB�H��group d on d.�N��=a.ASW_CB
where a.TxDate=@y --and a.ASW_POS_TYPE='PROP'
group by d.�H������


insert @�H�����ŭ��Btemp 
select case when d.�H������ in('6','7') then '6+' else d.�H������ end
,sum(a.�Ѽ�*100*d.��O���)/1000000.
 as MV
from PL.[dbo].[�Ũ�L�a����] a
join cb.dbo.CBData b on b.TxDate=@t and b.�i��ťN�X=a.�N��
left join @CB�H��group d on d.�N��=a.�N��
where a.TxDate=(select max(Txdate) from PL.[dbo].[�Ũ�L�a����] where a.Tag='CBAS')
group by case when d.�H������ in('6','7') then '6+' else d.�H������ end



insert @�H�����ŭ��B
select �H��,sum(���쭱�B) from @�H�����ŭ��Btemp
group by �H��

-- 2017/5/15 William �[�J�A�]���H��99�N���O�L�H�����šA�ҥH��6+���N

--select * from @�H�����ŭ��B
--select * from @CB�H��group 

declare @�H���`�M dec(20,5) set @�H���`�M = (select sum(���쭱�B)  from @�H�����ŭ��B)

declare @�H�� table(
�H�� nvarchar(20), ���� dec(20,5), �H�����ŭ��B_�ʸU dec(20,5) , ���B�ʤ��� dec(20,5), �`���B�ʤ��� dec(20,5)
, �`���B�W����� dec(20,5), �`���B�W���Ұʪ��e_�ʸU dec(20,5), �Ұ� nvarchar(20), �W�� nvarchar(20) 
)
insert @�H�� 
select   b.�����H���ά������ as �H��,a.���쭱�B
,b.�H�����ŭ��B_�ʸU
,a.���쭱�B/b.�H�����ŭ��B_�ʸU
,a.���쭱�B/@�H���`�M
 as �ʤ���
 , b.�`���B�W�����
 , b.�`���B�W���Ұʪ��e_�ʸU
 , case when @�H���`�M > b.�`���B�W���Ұʪ��e_�ʸU then '�Ұ�' else '' end
 , case when b.�H�����ŭ��B_�ʸU < a.���쭱�B then '�W��'
 when (@�H���`�M > b.�`���B�W���Ұʪ��e_�ʸU) and (a.���쭱�B/@�H���`�M >= b.�`���B�W�����) then '�W��'
 end 

from[DBMain].[dbo].����겣�洫CBAS_Credit�ݳ��쭭�B b 
left join @�H�����ŭ��B a on
b.�����H���ά������=(case when a.�H�� in('6','7') then '6+' 
                           when a.�H��='99' then '�L'
						   else a.�H�� end )
where b.ResetDate=@��Ƥ��
order by  b.�����H���ά������

--select * from @�H��

insert @�H��
select  '�`�M', @�H���`�M, (select max(�`���B�W��_�ʸU) from [DBMain].[dbo].����겣�洫CBAS_Credit�ݳ��쭭�B where ResetDate=@��Ƥ�� ), null, null,null,null,null, null

select * from @�H��
order by �H��


