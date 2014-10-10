-- query to generate all parent-child pairs
select   r.Child_id child_id, r.Parent_id parent_id,
         (yc.Dyear - yc.Byear) child_ad, (yp.Dyear - yp.Byear) parent_ad, -- ages of death
         yc.Byear child_yob, yp.Byear parent_yob, -- years of birth
         yc.Dyear child_yod, yp.Dyear parent_yod -- years of death
from     relationship r, years yc, years yp
where    r.Child_id = yc.Id
and      r.Parent_id = yp.Id
and      yc.Byear > -1 and yc.Dyear > -1 -- can't compute age of death if either birth year or death year is unknown (-1)
and      yp.Byear > -1 and yp.Dyear > -1
into outfile '/humgen/atgu1/fs03/eminikel/039famil/parent-child-pairs.txt'
;
