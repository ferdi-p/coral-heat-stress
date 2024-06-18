(* ::Package:: *)

(*Heat stress and bleaching in corals: a bioenergetic model*)
(*Pfab et al, 2024*)
(*define model*)


(*choose which fluxes are state variables*)
fluxvars={jCP,jSG};
(*derived*)
(*SU*)
f[max_][A_]:=(A max)/(A+max);
F[max_][A_,B_]:=(A B (A+B) max)/(B^2 max+A^2 (B+max)+A B (B+max));
sigmoidFunction=sigmax E^(-steepness(T-ED50))/(1+E^(-steepness(T-ED50)));
rules={
jX->(jXm X)/(X+KX),
jN->(jNm \[CapitalNu])/(\[CapitalNu]+KN),
jNH->(jN+nNX jX+rNH),
jHT->j0HT,
rNS->\[Sigma]NS nNS j0ST,
jHG->F[jHGm][yC jHC,jNH nNH^-1],
jHC->jX+S/H \[Rho]C,
jeC->jHC-jHG/yC,
jCO2->jeC kCO2,
jL->A astar L,
rCH->(jHT+(jHG (1-yC))/yC) \[Sigma]CH,
jeL->jL-jCP/yCL,
jNPQ->1/(1/jeL+1/kNPQ),
jST->(1+ b cROS1) j0ST ,
rNH->\[Sigma]NH nNH jHT,
A->1.256307+1.385969 E^(-6.479055 S/H),
rCS->\[Sigma]CS(j0ST+(1-yC)jSG yC^-1),
cROS1->Max[0,jeL-jNPQ]OneOverkROS,
\[Rho]C->jCP-jSG yC^-1,
\[Rho]N->jNH-jHG nNH,
jSG->F[jSGm][jCP yC,(rNS+H/S \[Rho]N)/nNS],
jCP->F[jCPm][jL yCL,rCS+(jCO2+rCH) H/S]/(1+cROS1),
jHGm->\[Alpha] jHGm0,
jNm->\[Alpha] jNm0,
jSGm->\[Alpha] jSGm0,
j0HT->\[Alpha] j0HT0,
j0ST->\[Alpha] j0ST0,
jXm->\[Alpha] jXm0,
(*NEED TO FIX THIS?*)
jCPm->\[Alpha] \[Beta] jCPm0,
\[Alpha]->Q10^((T-T0)/10),
\[Beta]->sigmoidFunction/(sigmoidFunction/.T->T0)
};
states=Join[fluxvars,{H,S}];
rulesnofluxvars=Select[rules,Not[MemberQ[fluxvars,#[[1]]]]&];
rulesnofluxvars=(#[[1]]->(#[[2]]/.rulesnofluxvars/.rulesnofluxvars)&/@rulesnofluxvars);


timetostate=((#->#[t])&/@states);
Clear[simulate];
simulate[pars_,(*environment_,*)fluxvars_,inivalues_]:=
simulate[pars,(*environment,*)fluxvars,inivalues]=
Module[{tsolve,eqs,inis},
(*list of states (fluxes) to track*)
(*format of elements in this list:*)
(*{ variable name, formula (aim for numerical implementation), initial guess (for numerical implementation) } *)  
tsolve=Select[rules,MemberQ[fluxvars,#[[1]]]&];
(*put together equations*)
eqs=(#[[1]]==(#[[2]]//.rulesnofluxvars/.timetostate(*/.environment*)))&/@Join[
(#[[1]]'[t]==(\[Lambda](#[[2]]-#[[1]])))&/@tsolve,
{

S'[t]==S(jSG-jST),
H'[t]==H(jHG-jHT)
}
];
(*set initial conditions*)
inis=(#[tmin/.pars]==(#/.inivalues))&/@states;
(*do the simulation (solve ODEs)*)
(NDSolve@@({Join[eqs/.pars,inis],#&/@states,{t,tmin,tmax},Method->Automatic}/.pars))[[1]]];
(*simulate[parvalsFvFmArr,environment,fluxvars,inivalsdefault];*)
