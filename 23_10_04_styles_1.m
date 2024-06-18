(* ::Package:: *)

(*Heat stress and bleaching in corals: a bioenergetic model*)
(*Pfab et al, 2024*)
(*load functions and options for simulations and plots*)


(*defines names and units*)
namesLong=#[[1]]->Column[#[[2]],Alignment->Center]&/@{
b cROS1 j0ST->{"symbiont expulsion","b(\!\(\*SubscriptBox[\(c\), \(ROS\)]\)-1)\!\(\*SubscriptBox[\(j\), \(ST, 0\)]\)"},
cROS1->{"reactive oxygen species","\!\(\*SubscriptBox[\(c\), \(ROS\)]\)-1"},
S/H->{"symbiont to host ratio","S/H"},
\[Rho]C->{"carbon shared by symbiont","\!\(\*SubscriptBox[\(\[Rho]\), \(C\)]\)"},
\[Rho]N->{"nitrogen shared by host","\!\(\*SubscriptBox[\(\[Rho]\), \(N\)]\)"},
jCO2->{"\!\(\*SubscriptBox[\(CO\), \(2\)]\) concentrated by host","\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CO\), \(2\)]]\)"},
jSG-jST->{"symbiont net growth rate","S'/S"},
jHG-jHT->{"host net growth rate","H'/H"},
jHG->{"host gross growth rate","\!\(\*SubscriptBox[\(j\), \(HG\)]\)"},
jSG->{"symbiont gross growth rate","\!\(\*SubscriptBox[\(j\), \(SG\)]\)"},
jHG->{"host gross growth rate","\!\(\*SubscriptBox[\(j\), \(HG\)]\)"},
jCP->{"realized photosynthesis rate","\!\(\*SubscriptBox[\(j\), \(CP\)]\)"},
jCPm->{"maximal photosynth. rate","\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)"},
arr->{"metabolic speed","\[Alpha]"},
jeL->{"excess light","\!\(\*SubscriptBox[\(j\), \(eL\)]\)"},
S->{"symbiont biomass","S"},
H->{"host biomass","H"},
T->{"temperature","T"},
X->{"prey","X"},
\[CapitalNu]->{"DIN concentration","N"},
10^6  X->{"prey","X"},
10^6  \[CapitalNu]->{"DIN concentration","N"},
L->{"light","L"}
};
namesShort={
cROS1+1->"\!\(\*SubscriptBox[\(c\), \(ROS\)]\)",
jL yCL->"\!\(\*SubscriptBox[\(j\), \(CP\)]\): L",
rCS+(Abs@H/Abs@S) (jCO2+rCH)+CO20->"\!\(\*SubscriptBox[\(j\), \(CP\)]\): \!\(\*SubscriptBox[\(CO\), \(2\)]\)",
jCP yC->"\!\(\*SubscriptBox[\(j\), \(SG\)]\): C",(rNS+\[Rho]N (Abs@H/Abs@S))/nNS->"\!\(\*SubscriptBox[\(j\), \(SG\)]\): N",
yC (\[Rho]C (Abs@S/Abs@H)+jX)->"\!\(\*SubscriptBox[\(j\), \(HG\)]\): C",
(1/(1+\[Kappa] S/H) )jNH nNH^-1->"\!\(\*SubscriptBox[\(j\), \(HG\)]\): N",
H/S->"H/S",
S/H->"\!\(\*StyleBox[\"S\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"/\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"H\",FontSlant->\"Italic\"]\)",
jHG->"\!\(\*SubscriptBox[\(j\), \(HG\)]\)",
\[Rho]N->"\!\(\*SubscriptBox[\(\[Rho]\), \(N\)]\)",
jeC->"\!\(\*SubscriptBox[\(j\), \(eC\)]\)",
jCO2->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CO\), \(2\)]]\)",
rCH->"\!\(\*SubscriptBox[\(r\), \(CH\)]\)",
rCS->"\!\(\*SubscriptBox[\(r\), \(CS\)]\)",
jCP->"\!\(\*SubscriptBox[\(j\), \(CP\)]\)",
jeL->"\!\(\*SubscriptBox[\(j\), \(eL\)]\)",
jNPQ->"\!\(\*SubscriptBox[\(j\), \(NPQ\)]\)",
cROS1->"\!\(\*SubscriptBox[\(c\), \(ROS\)]\)-1",
jSG->"\!\(\*SubscriptBox[\(j\), \(SG\)]\)",
\[Rho]C->"\!\(\*SubscriptBox[\(\[Rho]\), \(C\)]\)",
jX->"\!\(\*SubscriptBox[\(j\), \(X\)]\)",
jST->"\!\(\*SubscriptBox[\(j\), \(ST\)]\)",
jHT->"\!\(\*SubscriptBox[\(j\), \(HT\)]\)",
jL->"\!\(\*SubscriptBox[\(j\), \(L\)]\)",
rNS->"\!\(\*SubscriptBox[\(r\), \(NS\)]\)",
rNH->"\!\(\*SubscriptBox[\(r\), \(NH\)]\)",(*
limit[x_][y_,z_]->Style[limit[x][y,z],4],*)
jCPm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)",
jSGm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(SG\), \(m\)]]\)",
jHGm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(HG\), \(m\)]]\)",
cROS->"\!\(\*SubscriptBox[\(c\), \(ROS\)]\)",
jXm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(X\), \(m\)]]\)",
jNm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(N\), \(m\)]]\)",
jHGm->"j\!\(\*SubscriptBox[StyleBox[\"HG\",FontSize->12], \"m\"]\)",
kNPQ->"\!\(\*SubscriptBox[\(k\), \(NPQ\)]\)",
kROS->"\!\(\*SubscriptBox[\(k\), \(ROS\)]\)",
jCPm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)"
,jSGm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(SG\), \(m\)]]\)",
j0HT->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(HT\), \(0\)]]\)",
j0ST->"\!\(\*SubscriptBox[SubscriptBox[\(j\), \(ST\)], \(0\)]\)",
b->"b",rCS+(Abs@H/Abs@S) (jCO2+rCH)->"\!\(\*SubscriptBox[\(j\), \(SG\)]\): \!\(\*SubscriptBox[\(CO\), \(2\)]\)",
OneOverkROS->"1/\!\(\*SubscriptBox[\(k\), \(ROS\)]\)",
jSG-jST->"\!\(\*StyleBox[\"S\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"'\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"/\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"S\",FontSlant->\"Italic\"]\)",
jHG-jHT->"\!\(\*StyleBox[\"H\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"'\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"/\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"H\",FontSlant->\"Italic\"]\)",
0.55 FvFmtolerance[w]->"\!\(\*SubscriptBox[\(F\), \(V\)]\)/\!\(\*SubscriptBox[\(F\), \(M\)]\)",
jCPm (*->"Subscript[j, Subscript[CP, m]]",*)->"photosy. capacity \!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)",
\[Alpha]->"metabolic speed \[Alpha]",
b cROS1 j0ST->"symbiont expulsion",
jeL->"excess light",
kCO2->"\!\(\*SubscriptBox[\(k\), SubscriptBox[\(CO\), \(2\)]]\)",
yCL->"\!\(\*SubscriptBox[\(y\), \(CL\)]\)",
yC->"\!\(\*SubscriptBox[\(y\), \(C\)]\)",
astar->"\!\(\*SuperscriptBox[OverscriptBox[\(a\), \(_\)], \(*\)]\)",
jHGm0->"\!\(\*SubsuperscriptBox[\(j\), SubscriptBox[\(HG\), \(m\)], \(0\)]\)",
jSGm0->"\!\(\*SubsuperscriptBox[\(j\), SubscriptBox[\(SG\), \(m\)], \(0\)]\)",
jNm0->"\!\(\*SubsuperscriptBox[\(j\), SubscriptBox[\(N\), \(m\)], \(0\)]\)",
jXm0->"\!\(\*SubsuperscriptBox[\(j\), SubscriptBox[\(X\), \(m\)], \(0\)]\)",
j0HT0->"\!\(\*SubsuperscriptBox[\(j\), \(HT, 0\), \(0\)]\)",
j0ST0->"\!\(\*SubsuperscriptBox[\(j\), \(ST, 0\), \(0\)]\)",
jCPm0->"\!\(\*SubsuperscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)], \(0\)]\)"
};

units={cROS1->"",
S/H->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\)",
\[Rho]C->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
\[Rho]N->"mol N mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCO2->"mol \!\(\*SubscriptBox[\(CO\), \(2\)]\) mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jSG-jST->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jHG-jHT->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jHG->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jSG->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCP->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCPm->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
b cROS1 j0ST->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
\[Alpha]->"",
jeL->"mol phot. mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
S->"",
H->"",
jHGm->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jSGm->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jNm->"mol N mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jXm->"mol C mol \!\(\*SuperscriptBox[\(C\), \(\(=\)\(1\)\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
j0HT->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
j0ST->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
kCO2->"mol \!\(\*SubscriptBox[\(CO\), \(2\)]\) mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
yCL->"mol C mol phot\!\(\*SuperscriptBox[\(.\), \(-1\)]\)",
yC->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\)",
astar->"mol C mol phot\!\(\*SuperscriptBox[\(.\), \(-1\)]\)",
kNPQ->"mol phot. mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
OneOverkROS->"(mol phot. mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)\!\(\*SuperscriptBox[\()\), \(-1\)]\)",
b->"-",
T->"\[Degree]C",
10^6 \[CapitalNu]->"\[Mu]mol N \!\(\*SuperscriptBox[\(L\), \(-1\)]\)",
10^6 X->"\[Mu]mol C \!\(\*SuperscriptBox[\(L\), \(-1\)]\)",
L->"mol photos \!\(\*SuperscriptBox[\(m\), \(-2\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jHGm0->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jSGm0->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jNm0->"mol N mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jXm0->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
j0HT0->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
j0ST0->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCPm0->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)"
};
names=Join[namesLong,namesShort];
SUnames={jHGm->"H growth SU",jSGm->"S growth SU", jCPm->"photosynth. SU"};
SUunits={
jHGm->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jSGm->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCPm->"mol C mol \!\(\*SuperscriptBox[\(C\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)"
};
AlternativeNamesforSU={jCPm ->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)"};
plotlabelsize=12;
plotcols=ColorData[97]/@Range[2];
fontfamily="Arial";
eqstyle[text_]:=Style[text,SingleLetterItalics->False,plotlabelsize];
unitlabelsize=9;
timelabel="day";
unitstyle[text_]:=Style[text,SingleLetterItalics->False,unitlabelsize];
timeplotstyle={AxesOrigin->{0,0},ImageSize->Small,PlotStyle->({#,Thickness[.02]}&/@plotcols),Axes->False,LabelStyle->{FontFamily->fontfamily,FontSize->12,Exclusions->None},ImagePadding->{{58,20},{40,5}},FrameTicks->{{Charting`ScaledTicks[{Identity,Identity}][##,{4,4}]&,None},{Charting`ScaledTicks[{Identity,Identity}][##,{4,4}]&,None}},PlotRange->Full,Frame->{{True,False},{True,False}},ClippingStyle->Automatic};


(*this functions plots quantity x, with parameter pars, ODE simulation sol, and plot options options*)
partplot[x_,pars_,sol_,options_]:=Plot[Evaluate@Flatten@{x//.(rulesnofluxvars)/.((#->#[t])&/@states)/.sol/.pars},{t,0,tmax/.pars},Evaluate@options,Evaluate@timeplotstyle,PlotLabel->eqstyle[(x/.names)],FrameLabel->{unitstyle[timelabel],unitstyle[x/.units]}];


arrow1=Graphics[{Thickness[.05 100/#],Arrowheads->.2 100/#,Arrow[{{.5,1},{.5,0}}]},ImageSize->#]&@35;
{arrow2,arrow3}=Table[Graphics[{Thickness[.05 100/Mean@#],Arrowheads-> .3 100/#,Arrow[BezierCurve[{{0,0},{direction,0},{direction,-1}}]]},ImageSize->#,ImagePadding->.05Mean@#]&@{90,70}(*{90,70}*),{direction,{-1,1}}];
framingRadius=5;
toplotShort={\[Alpha],jCPm,S/H};
PanelPlotShort[pars_,label_]:=Module[{sol=simulate[pars,fluxvars,inivalsdefault/.pars]},
Column[
{
Spacer[{1,2}],
Style[label,18,FontFamily->"Times New Roman"],
Spacer[{1,2}],
Framed[Column[{
partplot[
toplotShort[[1]],pars,sol,{}],
partplot[toplotShort[[2]],pars,sol,{}]
}],RoundingRadius->framingRadius],
arrow1,
Framed[Column[{
partplot[
toplotShort[[3]],pars,sol,PlotRange->{0,.26}]
}],RoundingRadius->framingRadius]
},Alignment->Center,Spacings->0
]
](*]*);


colorsmultsimus={PlotStyle->Table[{Thickness[0.02],\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Dashing", "[", ".03", "]"}], 
RowBox[{"i", "==", "101"}]},
{"Nothing", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\),ColorData[97][i]},{i,{4,15}}]};


(*plot temperature with multiple scenarios overlaying. styles: plotstyles; parss: list of paramter values, includes T-> for temperature that is plotted; plotrange: plotrange of plot*)
tempplots[styles_,parss_,plotrange_]:=Plot[Evaluate@Table[T/.pars,{pars,parss}],{t,0,Max[tmax/.parss]},AxesOrigin->Automatic,styles,PlotLabel->eqstyle[(T/.names)],PlotRange->plotrange,Evaluate@timeplotstyle,FrameLabel->{unitstyle[timelabel],unitstyle["\[Degree]C"]}];



(*plot multiple simulations. x: quantity to plot; parsANDsols: list of parameter values and ODE solutions; stlyes: plotstyles*)
partplots[x_,parsANDsols_,styles_]:=Plot[
Evaluate@Table[
Flatten@{x//.(rulesnofluxvars)/.((#->#[t])&/@states)/.parsANDsol},
{parsANDsol,parsANDsols}],
{t,0,Max[tmax/.parsANDsols]},
Evaluate@styles,Evaluate@timeplotstyle,PlotLabel->eqstyle[(x/.names)],FrameLabel->{unitstyle[timelabel],unitstyle[x/.units]}];
PanelPlotMultipleRuns[toplot1_,toplot2_,parvalss_,styles_,inis_,plotrange_]:=Module[{parvalsANDsols,finalreplace
},

parvalsANDsols=Table[Join[parvals,simulate[parvals,fluxvars,inis/.parvals]],{parvals,parvalss}];
Style[Column[
{
Column[{
Row[{
Spacer[100],
Framed[tempplots[styles,parvalss,(T/.Join[plotrange,{_->Automatic}])],RoundingRadius->framingRadius],
Spacer[10],
Module[{maxtemps=NMaximize[T/.#,t][[1]]&/@parvalss},
Column[{
Style["peak temperature",plotlabelsize,FontFamily->fontfamily],
Grid[Transpose[
{
Plot[.5,{t,0,1},PlotRange->{0,1},PlotStyle->(#/.Dashing[_]->Dashing[0.19]/.Thickness[_]->Thickness[.15]),ImageSize->20,Axes->None]&/@(PlotStyle/.styles),
Style[ToString@If[And@@((Ceiling[#]-#<.01)&/@maxtemps),Round@#,NumberForm[#,{3,1}]]&@#<>"\[Degree]C",plotlabelsize,FontFamily->fontfamily]&/@maxtemps(*{34,32}*)
}
],Alignment->{Left,Left}]
}
,Alignment->Center]
]
}],
arrow1,Framed[Row[Table[partplots[x,parvalsANDsols,styles(*,parvals*)],{x,toplot1}]],
RoundingRadius->framingRadius],arrow1,
Framed[Multicolumn[
Table[partplots[x,parvalsANDsols,styles(*,parvals*)],
{x,toplot2}],4, Appearance -> "Horizontal"],RoundingRadius->framingRadius]},
Alignment->Center,Spacings->0]&[Join[]
]
},
Alignment->Center
],LineBreakWithin->False]];


(*this function creates a rule to replace paramter values. Input replaces: in for {a->2,..}. Usage: {a->3,b->2}/.ReplacePars[{a->2}] gives output {a->2,b->2}*)
ReplacePars[replaces_]:=Table[(replace[[1]]->_)->(replace[[1]]->replace[[2]]),{replace,replaces}];
(*decide which quantities to show in composite plots*)
toplot1={\[Alpha],jCPm};
toplot2={jCP,\[Rho]C,\[Rho]N,jCO2,jeL,cROS1,b cROS1 j0ST,jSG,jHG,jSG-jST,jHG-jHT,S/H};
toplot=Join[toplot1,toplot2];


(*composite plot*)
PanelPlot[toplotEnvironment_,toplot1_,toplot2_,toplotPlayers_,toplotSUs_,pars_,inis_,plotrange_]:=Module[{sol,finalreplace},
sol=simulate[pars,fluxvars,inis/.pars];
Style[Column[
{
Framed[Row[#],RoundingRadius->framingRadius]&@Table[partplot[x,pars,{},{Ticks->{Charting`ScaledTicks[{Identity,Identity}][##,{4,4}]&,Automatic},PlotLabel->Style[(x/.names),plotlabelsize],PlotRange->(x/.Join[plotrange,{_->Automatic}]),Evaluate@timeplotstyle(*,ScalingFunctions->"Log"*)}],{x,toplotEnvironment}],
arrow1,
Framed[Row[#],RoundingRadius->framingRadius]&@Table[partplot[x,pars,sol,{}],{x,toplot1}],
arrow1,
Framed[
Column[{
Multicolumn[#,4,Appearance ->"Horizontal"]&@Table[partplot[x,pars,sol,{}],{x,toplot2}],
Row[#]&@Table[Plot[
Evaluate[((SUins//.(rulesnofluxvars))/.pars/.((#->#[t])&/@states)/.sol/.pars)],
{t,0,tmax/.pars},(*
PlotPoints->10000,
Exclusions->None,*)
AxesOrigin->{0,0},
PlotLabel->Style[(SUins[[1]]/.SUnames),plotlabelsize],
PlotStyle->Thick,
PlotRange->(SUins[[1]]/.{jHGm->{0,jHGmmax},jSGm->{0,jSGmmax}, jCPm->{0,jCPmmax},_->Automatic}/.plotrange),
FrameLabel->{unitstyle[timelabel],unitstyle[SUins[[1]]/.SUunits]},
Evaluate@timeplotstyle,
PlotLegends->Placed[({SUins[[1]],\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Column", "[", 
RowBox[{
RowBox[{"{", 
RowBox[{"\"\<effective\>\"", ",", "\"\<light\>\""}], "}"}], ",", 
RowBox[{"Alignment", "->", "Center"}]}], "]"}], 
RowBox[{
RowBox[{"SUins", "[", 
RowBox[{"[", "1", "]"}], "]"}], "==", "jCPm"}]},
{"\"\<C\>\"", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\),\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{"\"\<\!\(\*SubscriptBox[\(CO\), \(2\)]\)\>\"", 
RowBox[{
RowBox[{"SUins", "[", 
RowBox[{"[", "1", "]"}], "]"}], "==", "jCPm"}]},
{"\"\<N\>\"", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)}/.AlternativeNamesforSU/.namesShort),Right],
ClippingStyle->Automatic],{SUins,toplotSUs}]
},Alignment->Center],
RoundingRadius->framingRadius]
},
Alignment->Center,Spacings->0
],LineBreakWithin->False]];
toplotSUs={{jHGm,yC (\[Rho]C (Abs@S/Abs@H)+jX),jNH nNH^-1},{jSGm,jCP yC,(rNS+\[Rho]N (Abs@H/Abs@S))/nNS},{jCPm ,jL yCL,(Abs@H/Abs@S) (jCO2+rCH)}};


(*composite plot, with multiple scenarios overlaying*)
PanelPlotMultipleParametersExtraPlotRanges={S/H->{0,.25},jCO2->{0,1.1},_->Automatic};
PanelPlotMultipleParameters[toplot1_,toplot2_,parsS_,inis_,plotrange_]:=Module[{sols(*,parsS=parvalsS*),timeplotstyleS},

sols=Table[simulate[pars,(*environment,*)fluxvars,inis/.pars],{pars,Reverse@parsS}];
timeplotstyleS=Table[With[{ps=PlotStyle/.timeplotstyle},(timeplotstyle/.(PlotStyle->x_)->(PlotStyle->ps[[i]]))],{i,Range@Length@parsS}];
Style[Column[
{
Framed[Row[#],RoundingRadius->framingRadius]&@Table[Show[Table[partplot[x,(*environment,*)parsS[[i]],{},{FrameTicks->{Charting`ScaledTicks[{Identity,Identity}][##,{4,4}]&,Automatic},PlotLabel->Style[(x/.names),plotlabelsize],PlotRange->(x/.Join[plotrange,{_->Automatic}]),Evaluate@timeplotstyleS[[i]]}],{i,Reverse@Range@Length@parsS}]],{x,toplot1}],
arrow1,(*
Framed[Row[#],RoundingRadius->framingRadius]&@Table[Show[Table[partplot[x,environment,parsS[[i]],sols[[i]],{Evaluate@timeplotstyleS[[i]]}],{i,Length@parsS}]],{x,toplot1}],
arrow1,*)
Framed[
Multicolumn[#,4,Appearance ->"Horizontal"]&@Table[Show[Table[partplot[x,(*environment,*)parsS[[i]],sols[[i]],{PlotRange->(x/.PanelPlotMultipleParametersExtraPlotRanges),Evaluate@(Reverse@timeplotstyleS)[[i]]}],{i,Reverse@Range@Length@parsS}](*,PlotRange->All*)(*,FrameTicks->Automatic*)],{x,toplot2}],
RoundingRadius->framingRadius]
},
Alignment->Center,Spacings->0
],LineBreakWithin->False]];
SUmaxs={jHGmmax->.5,jSGmmax->1.6,jCPmmax->14};
Srangepw={H->{2,100},S->{.4,50 .4}};
PWplot[filename_,parvals_,ranges_]:=(Export[NotebookDirectory[]<>"export/"<>filename,#];#)&@
PanelPlot[{T},toplot1,toplot2,{},toplotSUs,parvals,inivalsdefault,ranges];


(*functino to find final states by simulation*)
Clear[FinalStates];
FinalStates[pars_,fluxvars_,inivalues_]:=FinalStates[pars,fluxvars,inivalues]=Module[{sol},
sol=simulate[pars,fluxvars,inivalues/.pars];
Table[state->(state[tmax/.pars]/.sol),{state,states}]
];


(*plot of final states in dependence of a paramter value*)
FinalStatePlotLabel={T->"\[Degree]C",\[CapitalNu]->"DIN: \[Mu]mol N \!\(\*SuperscriptBox[\(L\), \(-1\)]\)",X->"Prey: \[Mu]mol C \!\(\*SuperscriptBox[\(L\), \(-1\)]\)",L->"Light: mol photos \!\(\*SuperscriptBox[\(m\), \(-2\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)"};
Clear[FinalStatePlot];
FinalStatePlot[parvals_,toplotys_,parameter_,parameterrange_,inivals_,legend_,plotrange_,options_,parxAxisMult_]:=Module[{list=Table[
Table[Module[{
parvals2=Join[{parameter->parametervalue},parvals]
},
{parxAxisMult parameter/.parvals2,Evaluate[toploty//.(rulesnofluxvars)/.parvals2/.FinalStates[parvals2,fluxvars,inivals]/.t->(tmax/.parvals2)]}],{parametervalue,parameterrange}],{toploty,toplotys}]},
ListPlot[list,Evaluate@options,PlotRange->(toplotys[[1]]/.plotrange),Joined->True,AxesOrigin->Automatic,ImageSize->Small,PlotStyle->If[Length@list==3,({#,AbsoluteThickness[2]}&/@(ColorData[97]/@Range[3])),{Darker@Brown,AbsoluteThickness[2]}],LabelStyle->{FontFamily->fontfamily,FontSize->12},PlotLegends->legend,PlotRange->Full,PlotRangeClipping->None,ClippingStyle->Automatic,PlotLabel->eqstyle[toplotys[[1]]/.\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{"names", 
RowBox[{
RowBox[{"Length", "[", "toplotys", "]"}], "==", "1"}]},
{"SUnames", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)],FrameLabel->{{unitstyle[toplotys[[1]]/.\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{"units", 
RowBox[{
RowBox[{"Length", "[", "toplotys", "]"}], "==", "1"}]},
{"SUunits", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)],None},{unitstyle[parameter/.FinalStatePlotLabel],None}},(*ImagePadding\[Rule]{{55,100},{35,5}},*)Evaluate@timeplotstyle]
];
ticksmanual[min_,max_]:=N@FindDivisions[{0,max},3];
arrangementFinalStatePlot=Column[{Framed[Row[#1[[{1,2}]]],RoundingRadius->framingRadius],arrow1,Framed[Column[{Multicolumn[#1[[3;;-4]],4,Appearance->"Horizontal",Alignment->Center],Row[#1[[-3;;-1]]]},Alignment->Center],RoundingRadius->framingRadius]},Alignment->Center,Spacings->0]&;
finalplot[filename_,parvals_,(*environment_,*)parameter_,parameterrange_,inivals_,yranges_,arrangement_,optionss_,parxAxisMult_]:=(Export[NotebookDirectory[]<>"export/"<>filename,#];#)&@
Style[arrangement@
Join[Insert[Table[
FinalStatePlot[parvals,(*environment,*){x},parameter,parameterrange,inivals,None,{_->Full},x/.optionss,parxAxisMult]
,{x,toplot}],Nothing,-3],
Table[
FinalStatePlot[parvals,(*environment,*)SUins,parameter,parameterrange,inivals,Placed[({SUins[[1]],\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Column", "[", 
RowBox[{
RowBox[{"{", 
RowBox[{"\"\<effective\>\"", ",", "\"\<light\>\""}], "}"}], ",", 
RowBox[{"Alignment", "->", "Center"}]}], "]"}], 
RowBox[{
RowBox[{"SUins", "[", 
RowBox[{"[", "1", "]"}], "]"}], "==", "jCPm"}]},
{"\"\<C\>\"", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\),\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{"\"\<\!\(\*SubscriptBox[\(CO\), \(2\)]\)\>\"", 
RowBox[{
RowBox[{"SUins", "[", 
RowBox[{"[", "1", "]"}], "]"}], "==", "jCPm"}]},
{"\"\<N\>\"", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)}/.AlternativeNamesforSU/.names),Right],yranges,x/.optionss,parxAxisMult]
,{SUins,toplotSUs}]],LineBreakWithin->False];


(*plot of final states in dependence of two parameter values*)
FinalState2DDiscretePlot[parvals_,inivals_,ConditionsAndColors_,parX_,parY_,rangeX_,rangeY_,contours_,ticksX_,ticksY_,legend_,epilog_,names_]:=Module[{
list,x,y
},
list=Table[
Module[{
parvals2=Join[{parY->valueY,parX->valueX},parvals]},
list=Evaluate@Piecewise[Table[
{
 i,

ConditionsAndColors[[i,1]]//.(rulesnofluxvars)/.parvals2/.FinalStates[parvals2,fluxvars,inivals]},
{i,Length@ConditionsAndColors}]]
],
{valueX,rangeX},
{valueY,rangeY}
];
Grid[{{
ArrayPlot[
list,
Ticks->None,
ColorFunctionScaling->False,
ColorFunction->Function[arg,Piecewise[Table[{ConditionsAndColors[[i,2]],arg==i},{i,Length@ConditionsAndColors}]]],
FrameTicks->{ticksX,ticksY},
FrameLabel->(eqstyle/@({"\[Degree]C",parY}/.names)),
DataReversed->True,
Frame->True,
Mesh->False,
ImageSize->220,
ImagePadding->{{50,10},{50,5}},
LabelStyle->12,
Epilog->epilog
],
Column[{Spacer[{0,20}],If[legend,legendFinalState2DDiscretePlot[ConditionsAndColors],Nothing]}]
}},Alignment->Top]
];
makeTicks[range_,ticknumberaim_]:=Table[{i,N@range[[i]] },{i,1,Length@range,Round[Length@range/ticknumberaim]}];
(*threshold for bleached corals*)
bleachedSH=0.02;
ConditionsAndColors={
{S/H>bleachedSH\[And]H>(H0/.parvalsFvFmArr),ColorData[97][15],"not bleached, growing coral"},
{S/H<=bleachedSH\[And]H<=(H0/.parvalsFvFmArr),ColorData[97][4],"bleached, dying coral"},
{H<=(H0/.parvalsFvFmArr),ColorData[97][5],"not bleached, dying coral"},
{H>(H0/.parvalsFvFmArr)\[And]S/H<=bleachedSH,ColorData[97][8],"bleached, growing coral"}
};
ConditionsAndColors2={
{S/H>bleachedSH\[And]H>(H0/.parvalsFvFmArr),ColorData[97][15],Column[{"not bleached, growing coral"},Alignment->Center]},
{True,ColorData[97][4],Column[{"bleached and/or dying coral"},Alignment->Center]}
};
legendFinalState2DDiscretePlot[ConditionsAndColors_]:=Framed[SwatchLegend[ConditionsAndColors[[All,2]],ConditionsAndColors[[All,3]](*,LegendLayout\[Rule]"Grid"*)(*,LabelStyle\[Rule]{FontFamily->fontfamily}*)],RoundingRadius->10];
(*legendFinalState2DDiscretePlot[ConditionsAndColors2]*)
