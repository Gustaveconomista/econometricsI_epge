
use brdb.dta, clear

drop if ADMIN_NAME=="Litigated Zone"

gen inst_c=0.4481768 if ADMIN_NAME=="Acre"
replace inst_c=0.0718791 if ADMIN_NAME=="Alagoas"
replace inst_c=0.5444938 if ADMIN_NAME=="Amazonas"
replace inst_c=0.5981804 if ADMIN_NAME=="Amapa"
replace inst_c=0.3632319 if ADMIN_NAME=="Bahia"
replace inst_c=0.4165474 if ADMIN_NAME=="Ceara"
replace inst_c=1 if ADMIN_NAME=="Distrito Federal"
replace inst_c=0.5625693 if ADMIN_NAME=="Espirito Santo"
replace inst_c=0.3886635 if ADMIN_NAME=="Goias"
replace inst_c=0.2972295 if ADMIN_NAME=="Maranhao"
replace inst_c=0.4609004 if ADMIN_NAME=="Minas Gerais"
replace inst_c=0.4614621 if ADMIN_NAME=="Mato Grosso do Sul"
replace inst_c=0.3511314 if ADMIN_NAME=="Mato Grosso"
replace inst_c=0.4281587 if ADMIN_NAME=="Para"
replace inst_c=0.2953183 if ADMIN_NAME=="Paraiba"
replace inst_c=0.4842924 if ADMIN_NAME=="Pernambuco"
replace inst_c=0.2258026 if ADMIN_NAME=="Piaui"
replace inst_c=0.4492917 if ADMIN_NAME=="Parana"
replace inst_c=0.8993561 if ADMIN_NAME=="Rio de Janeiro"
replace inst_c=0.3123622 if ADMIN_NAME=="Rio Grande do Norte"
replace inst_c=0.3648933 if ADMIN_NAME=="Rondonia"
replace inst_c=0.6315905 if ADMIN_NAME=="Roraima"
replace inst_c=0.5240926 if ADMIN_NAME=="Rio Grande do Sul"
replace inst_c=0.3744836 if ADMIN_NAME=="Santa Catarina"
replace inst_c=0.3345228 if ADMIN_NAME=="Sergipe"
replace inst_c=0.7631133 if ADMIN_NAME=="Sao Paulo"
replace inst_c=0.1472582 if ADMIN_NAME=="Tocantins"
replace inst_c=1 if ADMIN_NAME=="Alagoas"
replace inst_c=1 if ADMIN_NAME=="Rio de Janeiro"
replace inst_c=1 if ADMIN_NAME=="Amapa"
replace inst_c=1 if ADMIN_NAME=="Roraima"

spmap inst_c using brcoord.dta, id(id) fcolor(Blues)  clnumber(10) point(data(ev_coord.dta) xcoord(longitude) ycoord(latitude) size(vsmall) fcolor(cranberry))

graph export map_inst.eps, replace

