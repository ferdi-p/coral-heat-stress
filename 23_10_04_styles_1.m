(* ::Package:: *)

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
H->{"host biomass","H"}
};

namesShort={cROS1+1->"\!\(\*SubscriptBox[\(c\), \(ROS\)]\)",jL yCL->"\!\(\*SubscriptBox[\(j\), \(CP\)]\): L",rCS+(Abs@H/Abs@S) (jCO2+rCH)+CO20->"\!\(\*SubscriptBox[\(j\), \(CP\)]\): \!\(\*SubscriptBox[\(CO\), \(2\)]\)",jCP yC->"\!\(\*SubscriptBox[\(j\), \(SG\)]\): C",(rNS+\[Rho]N (Abs@H/Abs@S))/nNS->"\!\(\*SubscriptBox[\(j\), \(SG\)]\): N",yC (\[Rho]C (Abs@S/Abs@H)+jX)->"\!\(\*SubscriptBox[\(j\), \(HG\)]\): C",(1/(1+\[Kappa] S/H) )jNH nNH^-1->"\!\(\*SubscriptBox[\(j\), \(HG\)]\): N",H/S->"H/S",S/H->"\!\(\*StyleBox[\"S\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"/\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"H\",FontSlant->\"Italic\"]\)",jHG->"\!\(\*SubscriptBox[\(j\), \(HG\)]\)",\[Rho]N->"\!\(\*SubscriptBox[\(\[Rho]\), \(N\)]\)",jeC->"\!\(\*SubscriptBox[\(j\), \(eC\)]\)",jCO2->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CO\), \(2\)]]\)",rCH->"\!\(\*SubscriptBox[\(r\), \(CH\)]\)",rCS->"\!\(\*SubscriptBox[\(r\), \(CS\)]\)",jCP->"\!\(\*SubscriptBox[\(j\), \(CP\)]\)",jeL->"\!\(\*SubscriptBox[\(j\), \(eL\)]\)",jNPQ->"\!\(\*SubscriptBox[\(j\), \(NPQ\)]\)",cROS1->"\!\(\*SubscriptBox[\(c\), \(ROS\)]\)-1",jSG->"\!\(\*SubscriptBox[\(j\), \(SG\)]\)",\[Rho]C->"\!\(\*SubscriptBox[\(\[Rho]\), \(C\)]\)",jX->"\!\(\*SubscriptBox[\(j\), \(X\)]\)",jST->"\!\(\*SubscriptBox[\(j\), \(ST\)]\)",jHT->"\!\(\*SubscriptBox[\(j\), \(HT\)]\)",jL->"\!\(\*SubscriptBox[\(j\), \(L\)]\)",rNS->"\!\(\*SubscriptBox[\(r\), \(NS\)]\)",rNH->"\!\(\*SubscriptBox[\(r\), \(NH\)]\)",limit[x_][y_,z_]->Style[limit[x][y,z],4],jCPm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)",jSGm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(SG\), \(m\)]]\)",jHGm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(HG\), \(m\)]]\)",cROS->"\!\(\*SubscriptBox[\(c\), \(ROS\)]\)",jXm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(X\), \(m\)]]\)",jNm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(N\), \(m\)]]\)",jHGm->"j\!\(\*SubscriptBox[StyleBox[\"HG\",FontSize->12], \"m\"]\)",kNPQ->"\!\(\*SubscriptBox[\(k\), \(NPQ\)]\)",kROS->"\!\(\*SubscriptBox[\(k\), \(ROS\)]\)",jCPm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)",jSGm->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(SG\), \(m\)]]\)",j0HT->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(HT\), \(0\)]]\)",j0ST->"\!\(\*SubscriptBox[SubscriptBox[\(j\), \(ST\)], \(0\)]\)",b->"b",rCS+(Abs@H/Abs@S) (jCO2+rCH)->"\!\(\*SubscriptBox[\(j\), \(SG\)]\): \!\(\*SubscriptBox[\(CO\), \(2\)]\)",OneOverkROS->"1/\!\(\*SubscriptBox[\(k\), \(ROS\)]\)",jSG-jST->"\!\(\*StyleBox[\"S\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"'\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"/\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"S\",FontSlant->\"Italic\"]\)",jHG-jHT->"\!\(\*StyleBox[\"H\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"'\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"/\",FontSlant->\"Italic\"]\)\!\(\*StyleBox[\"H\",FontSlant->\"Italic\"]\)",0.55 FvFmtolerance[w]->"\!\(\*SubscriptBox[\(F\), \(V\)]\)/\!\(\*SubscriptBox[\(F\), \(M\)]\)",jCPm (*->"Subscript[j, Subscript[CP, m]]",*)->"photosy. capacity \!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)",arr->"metabolic speed \[Alpha]",b cROS1 j0ST->"symbiont expulsion",jeL->"excess light"};
units={cROS1->"",
S/H->"C-mol S C-mol \!\(\*SuperscriptBox[\(H\), \(-1\)]\)",
\[Rho]C->"mol C C-mol \!\(\*SuperscriptBox[\(S\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
\[Rho]N->"mol N C-mol \!\(\*SuperscriptBox[\(H\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCO2->"mol \!\(\*SubscriptBox[\(CO\), \(2\)]\) C-mol \!\(\*SuperscriptBox[\(H\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jSG-jST->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jHG-jHT->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jHG->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jSG->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCP->"mol C C-mol \!\(\*SuperscriptBox[\(S\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCPm->"mol C C-mol \!\(\*SuperscriptBox[\(S\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
b cROS1 j0ST->"\!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
arr->"",
jeL->"mol phot. C-mol \!\(\*SuperscriptBox[\(S\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
S->"",
H->""};
names=Join[namesLong,namesShort];
SUnames={jHGm->"H growth SU",jSGm->"S growth SU", jCPm->"photosynth. SU"};
SUunits={
jHGm->"C-mol H C-mol \!\(\*SuperscriptBox[\(H\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jSGm->"C-mol S C-mol \!\(\*SuperscriptBox[\(S\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)",
jCPm->"mol C C-mol \!\(\*SuperscriptBox[\(S\), \(-1\)]\) \!\(\*SuperscriptBox[\(d\), \(-1\)]\)"
};
AlternativeNamesforSU={jCPm ->"\!\(\*SubscriptBox[\(j\), SubscriptBox[\(CP\), \(m\)]]\)"};
plotlabelsize=12;
plotcols=ColorData[97]/@Range[2];
fontfamily="Arial";
eqstyle[text_]:=Style[text,SingleLetterItalics->False,plotlabelsize];
unitlabelsize=9;
timelabel="day";
unitstyle[text_]:=Style[text,SingleLetterItalics->False,unitlabelsize];
timeplotstyle={AxesOrigin->{0,0},ImageSize->Small,PlotStyle->({#,Thickness[.02]}&/@plotcols),Axes->False,LabelStyle->{FontFamily->fontfamily,FontSize->12,Exclusions->None},ImagePadding->{{55,25},{35,5}},FrameTicks->{{Charting`ScaledTicks[{Identity,Identity}][##,{4,4}]&,None},{Charting`ScaledTicks[{Identity,Identity}][##,{4,4}]&,None}},PlotRange->Full,Frame->{{True,False},{True,False}}};
tempplot[environment_,pars_,plotrange_]:=Plot[w/.environment,{t,0,tmax/.pars},AxesOrigin->Automatic,(*Ticks->{Charting`ScaledTicks[{Identity,Identity}][##,{4,4}]&,Automatic},*)PlotLabel->Style["temperature"(*Column[{"temperature","\[Degree]C"},Alignment\[Rule]Center]*),plotlabelsize],PlotRange->plotrange,Evaluate@timeplotstyle,FrameLabel->{unitstyle[timelabel],unitstyle["\[Degree]C"]}];
partplot[x_,environment_,pars_,sol_,options_]:=Plot[Evaluate@Flatten@{x//.(rulesnofluxvars)/.((#->#[t])&/@states)/.sol/.pars/.environment},{t,0,tmax/.pars},Evaluate@options,Evaluate@timeplotstyle,PlotLabel->eqstyle[(x/.names)],FrameLabel->{unitstyle[timelabel],unitstyle[x/.units]}];


(* ::Text:: *)
(**)


arrow1=Graphics[{Thickness[.05 100/#],Arrowheads->.2 100/#,Arrow[{{.5,1},{.5,0}}]},ImageSize->#]&@35;
{arrow2,arrow3}=Table[Graphics[{Thickness[.05 100/Mean@#],Arrowheads-> .3 100/#,Arrow[BezierCurve[{{0,0},{direction,0},{direction,-1}}]]},ImageSize->#,ImagePadding->.05Mean@#]&@{90,70}(*{90,70}*),{direction,{-1,1}}];


framingRadius=5;
toplotShort={Q10^(1/10 (w-w0)),jCPm,S/H};
PanelPlotShort[parameters_,environment_,label_]:=With[{pars=parameters/.environment},Module[{sol=simulate[pars,environment,fluxvars,inivalsdefault/.pars]},
Column[
{
Spacer[{1,2}],
Style[label,18,FontFamily->"Times New Roman"],
Spacer[{1,2}],
Framed[Column[{
partplot[
toplotShort[[1]],environment,pars,sol,{}],
partplot[toplotShort[[2]],environment,pars,sol,{}]
}],RoundingRadius->framingRadius],
arrow1,
Framed[Column[{
partplot[
toplotShort[[3]],environment,pars,sol,PlotRange->{0,.26}]
}],RoundingRadius->framingRadius]
},Alignment->Center,Spacings->0
]

]];


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


tempplots[environments_,styles_,pars_,plotrange_]:=Plot[Evaluate@Table[w/.environment,{environment,environments}],{t,0,tmax/.pars},AxesOrigin->Automatic,styles,PlotLabel->Style["temperature",plotlabelsize],PlotRange->plotrange,Evaluate@timeplotstyle,FrameLabel->{unitstyle[timelabel],unitstyle["\[Degree]C"]}];



partplots[x_,environmentsandsols_,styles_,pars_]:=Plot[Evaluate@Table[Flatten@{x//.(rulesnofluxvars)/.((#->#[t])&/@states)/.pars/.environmentandsol},{environmentandsol,environmentsandsols}],{t,0,tmax/.pars},Evaluate@styles,Evaluate@timeplotstyle,PlotLabel->eqstyle[(x/.names)],FrameLabel->{unitstyle[timelabel],unitstyle[x/.units]}];
PanelPlotMultipleRuns[toplot_,parvals_,environments_,styles_,inis_,{t1_,t2_},plotrange_,arrangement_]:=Module[{environmentsandsols,shadingplot,(*pars=parvals/.environment,*)finalreplace
},
shadingplot=Plot[\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox["10", "12"], 
RowBox[{"t1", "<", "t", "<", "t2"}]},
{"None", "True"}
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
StripWrapperBoxes->True]\),{t,0,tmax/.parvals},PlotStyle->Transparent,Filling->Bottom,FillingStyle->Opacity[0.1],PlotRange->{-10^12,10^12}];

environmentsandsols=Table[Join[environment,simulate[parvals/.environment,environment,fluxvars,inis/.parvals/.environment]],{environment,environments}];
Style[Column[
{(*alignment*)
Column[{Row[{Spacer[100],Framed[Row[#[[{1}]]],RoundingRadius->framingRadius],Spacer[10],
(*legend*)
Module[{maxtemps=NMaximize[w/.#,t][[1]]&/@environments},
Column[{Style["peak temperature",plotlabelsize,FontFamily->fontfamily],Grid[Transpose[{Plot[.5,{t,0,1},PlotRange->{0,1},PlotStyle->(#/.Dashing[_]->Dashing[0.19]/.Thickness[_]->Thickness[.15]),ImageSize->20,Axes->None]&/@(PlotStyle/.styles),Style[ToString@If[And@@((Ceiling[#]-#<.01)&/@maxtemps),Round@#,NumberForm[#,{3,1}]]&@#<>"\[Degree]C",plotlabelsize,FontFamily->fontfamily]&/@maxtemps(*{34,32}*)}],Alignment->{Left,Left}]},Alignment->Center]]
}],arrow1,Framed[Row[#[[{2,3}]]],RoundingRadius->framingRadius],arrow1,Framed[Multicolumn[#[[4;;-1]],4, Appearance -> "Horizontal"],RoundingRadius->framingRadius]},Alignment->Center,Spacings->0]&[Join[
(*the plots*)
{tempplots[environments,styles,parvals,(w/.Join[plotrange,{_->Automatic}])]},
Table[Show[{partplots[x,environmentsandsols,styles,parvals],shadingplot}],{x,toplot}]]
]
},
Alignment->Center
],LineBreakWithin->False]];


ReplacePars[replaces_]:=Table[(replace[[1]]->_)->(replace[[1]]->replace[[2]]),{replace,replaces}];
toplot={arr,jCPm, jCP,\[Rho]C,\[Rho]N,jCO2,jeL,cROS1,b cROS1 j0ST,jSG,jHG,jSG-jST,jHG-jHT,S/H};


SUmaxs={jHGmmax->.5,jSGmmax->1.6,jCPmmax->13};
Srangepw={H->{2,100},S->{.4,50 .4}};
arrangement=Column[{Framed[Row[#[[{1}]]],RoundingRadius->framingRadius],arrow1,Framed[Row[#[[{2,3}]]],RoundingRadius->framingRadius],arrow1,Framed[Column[{Multicolumn[#[[4;;-2]](*#[[{6,7,2,3}]]~Join~#[[8;;-2]]*),4, Appearance -> "Horizontal"],#[[-1]]}],RoundingRadius->framingRadius]},Alignment->Center,Spacings->0]&;
PWplot[filename_,parvals_,environment_,ranges_,shades_]:=(Export[NotebookDirectory[]<>"export/"<>filename,#];#)&@PanelPlot[toplot,{(*S,H*)},toplotSUs,parvals,environment,inivalsdefault,shades,ranges,arrangement];
(*simulate[parvalsFvFmArr,environment,fluxvars,inivalsdefault]*)


PanelPlot[toplot_,toplotPlayers_,toplotSUs_,parvals_,environment_,inis_,{t1_,t2_},plotrange_,arrangement_]:=Module[{sol,shadingplot,pars=parvals/.environment,finalreplace},
shadingplot=Plot[\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
SuperscriptBox["10", "12"], 
RowBox[{"t1", "<", "t", "<", "t2"}]},
{"None", "True"}
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
StripWrapperBoxes->True]\),{t,0,tmax/.pars},PlotStyle->Transparent,Filling->Bottom,FillingStyle->Opacity[0.1],PlotRange->{-10^12,10^12}];

sol=simulate[pars,environment,fluxvars,inis/.pars];
Style[Column[
{arrangement[Join[
{Show[{tempplot[environment,pars,(w/.Join[plotrange,{_->Automatic}])],shadingplot}]},
Table[Show[{Plot[Evaluate[x[t]/.sol],{t,0,tmax/.pars},Ticks->{Charting`ScaledTicks[{Identity,Identity}][##,{4,4}]&,Automatic},PlotLabel->Style[(x/.names),plotlabelsize],PlotRange->(x/.Join[plotrange,{_->Automatic}]),Evaluate@timeplotstyle,ScalingFunctions->"Log"],shadingplot}],{x,toplotPlayers}],
Table[Show[{partplot[x,environment,pars,sol,{}],shadingplot}],{x,toplot}],{Row[Table[Show[{Plot[Evaluate[((SUins//.(rulesnofluxvars))/.pars/.((#->#[t])&/@states)/.sol/.environment)],{t,0,tmax/.pars},AxesOrigin->{0,0},PlotLabel->Style[(SUins[[1]]/.SUnames),plotlabelsize],PlotStyle->Thick,PlotRange->(SUins[[1]]/.{jHGm->{0,jHGmmax},jSGm->{0,jSGmmax}, jCPm->{0,jCPmmax},_->Automatic}/.plotrange),FrameLabel->{unitstyle[timelabel],unitstyle[SUins[[1]]/.SUunits]},Evaluate@timeplotstyle,PlotLegends->({SUins[[1]],\!\(\*
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
StripWrapperBoxes->True]\)}/.AlternativeNamesforSU/.namesShort),ClippingStyle->Automatic],shadingplot}],{SUins,toplotSUs}]]}]
]
},
Alignment->Center
],LineBreakWithin->False]];
(*improve how health is tracked, plot accelerated rates*)
toplotSUs={{jHGm,yC (\[Rho]C (Abs@S/Abs@H)+jX),jNH nNH^-1},{jSGm,jCP yC,(rNS+\[Rho]N (Abs@H/Abs@S))/nNS},{jCPm ,jL yCL,(Abs@H/Abs@S) (jCO2+rCH)}};
