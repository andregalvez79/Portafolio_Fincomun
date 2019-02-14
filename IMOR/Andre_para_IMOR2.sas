  %LET fcv= fcv_2017_12_30_3;                   /*FCV BASE*/
        %LET flujo= fcv_2017_12_30_3;                   /*FCV FLUJO*/




proc sql;
create table &fcv as
select a.Oficina_Asesor, a.Prod, a.NoPBKS, a.SocioRea, a.Producto_Revisado, a.MtoVig2, a.MtoVenc2, a.IntVig2,
       a.IntVen2, a.PorcentProvision, a.TotProvision,a.Tipo, b.Territorio
from fcv.&fcv as a left join flujo.&flujo as b on a.NoPBKS=b.NoPBKS;
quit;

data &fcv;
set  &fcv;

where Oficina_Asesor not in (2,3,77,801,802);
 if PorcentProvision=1 then delete;
  if Producto_Revisado=4900 then delete;

Cartera_total=MtoVig2+MtoVenc2+IntVig2+IntVen2;
Cartera_vig=MtoVig2+IntVig2;
Cartera_ven=MtoVenc2+IntVen2;

length Tipox $20.;
if Tipo='Nuevo' then Tipox='Nuevo';
 else if Tipo='Redoc. Automatico' then Tipox='Recompra';
  else if Tipo='Redoc. no Automatico' then Tipox='Recompra';
   else if Tipo='Especiales' then Tipox='Recompra';
    else Tipox='';
run;
PROC EXPORT DATA= WORK.&fcv
            OUTFILE= "C:\Users\andrevargas\Documents\IMOR\BasesTerr\&fcv..csv"
            DBMS=CSV REPLACE;
RUN;
