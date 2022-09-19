(* ::Package:: *)

BeginPackage["MarbleSlide`"]
yvalue;
y;
init::usage=" "

Begin["`Private`"]


getIntersection[f1_,f2_,range1_:-11<=y<=11,range2_:-11<=y<=11]:= Return[Solve[{f1,f2,range1,range2},{x,y}]]


intersectLines[fun_,funArr_,rangeArr_,arrSlope_,currentRange_,ytmp_]:=(
Module[{result,index,temp,block},
	result = {-11,-11};
	index=-1;
	block=False;
For[i=1,i<=Length[funArr],i++,
 If[fun=!=funArr[[i]] && block==False,
	
    temp= getIntersection[fun,funArr[[i]],currentRange,rangeArr[[i]]];
    
	If[temp=!={}, 
	temp = {x,y} /. temp[[1]];
	If[temp[[2]]>result[[2]] && temp[[2]]<ytmp[[2]],
		result = temp;
		
		index = i;
		]
	(*If[temp == result,
		If[arrSlope[[index]]*arrSlope[[i]] <= 0,
			block=True;
			, 
			If[Abs[arrSlope[[i]]]<Abs[arrSlope[[index]]],
				index=i;]
			]
			];
	]*)
]]
]; 
Return [{result,index,block}];
]);


getValueRange[pos_,funIndex_,arrRange_]:=(
Module[{position},
position = pos;

If[pos == 2,position = 3];

Return[arrRange[[funIndex]][[position]]];
]);


findSolRoot[fun_, valx_]:=(
Return[fun[[2]]/.x->valx]
);


calculateRange[point_,funIndex_,arrRange_,arrSlope_]:=(

If[arrSlope[[funIndex]]=!= 0,
	If[arrSlope[[funIndex]]>0,
		Return[arrRange[[funIndex]][[1]]<=x<=point[[1]]];,
	Return[point[[1]]<=x<=arrRange[[funIndex]][[2]]];
	],	
	Return[-11<=x<=11]
];
);


CheckSlope[slope1_,slope2_]:=(
	Return[slope1*slope2 < 0 ||(slope1==0 && slope2==0)]; 
	 
)


calculatePath[arrFun_,arrRange_,arrSlope_,funOrdered_,rangeOrdered_,pointOrdered_]:=(
Module[{block=False,isFalling=True,notIntersection=False,
tempPoint,tempIndex,res,newRange,tempX,
currentIndex=1,currentFun=arrFun[[1]],currentRange=arrRange[[1]],currentPoint={arrFun[[1]][[2]],10},newPoint,tempBlock},

While[block===False && notIntersection===False ,
If[isFalling===False,
res = intersectLines[currentFun,arrFun,arrRange,arrSlope,currentRange,currentPoint];

If[res[[1]]=!={-11,-11},
tempPoint = res[[1]];
tempIndex = res[[2]];

If[(*res[[3]]==False &&*) Not[CheckSlope[arrSlope[[tempIndex]],arrSlope[[currentIndex]]]],

AppendTo[funOrdered,arrFun[[tempIndex]]];
,

AppendTo[funOrdered,0];
block =True;
];

AppendTo[pointOrdered,tempPoint];
newRange = tempPoint[[2]]<=y<=currentPoint[[2]];
AppendTo[rangeOrdered,newRange];
currentRange= calculateRange[tempPoint,currentIndex,arrRange,arrSlope];
currentFun = arrFun[[tempIndex]];
currentIndex = tempIndex;
currentPoint = tempPoint;

,
If[arrSlope[[currentIndex]]>0,
tempX = getValueRange[1,currentIndex,arrRange];

,
tempX = getValueRange[2,currentIndex,arrRange];

];


newPoint={tempX,findSolRoot[arrFun[[currentIndex]],tempX]};
AppendTo[rangeOrdered,newPoint[[2]]<=y<=currentPoint[[2]]];

currentPoint = newPoint;
AppendTo[pointOrdered,currentPoint];
isFalling =True;
AppendTo[funOrdered,x==tempX];
currentFun = x==tempX;
AppendTo[arrFun,currentFun];
AppendTo[arrRange,-11<=x<=currentPoint[[1]]];
AppendTo[arrSlope,0];

currentIndex = Length[arrFun];
currentRange = -11<=x<=currentPoint[[1]];
];
,
res= intersectLines[currentFun,arrFun,arrRange,arrSlope,currentRange,currentPoint];
If[res[[1]] =!={-11,-11},

tempPoint = res[[1]];
tempIndex = res[[2]];
(*tempBlock = res[[3]];*)

If[CheckSlope[arrSlope[[tempIndex]],arrSlope[[currentIndex]]] && (*tempBlock==True &&*) StringContainsQ[ToString[arrFun[[tempIndex]][[1]]],"y"],

AppendTo[funOrdered,0];
block = True;
,

isFalling = False;
AppendTo[funOrdered,arrFun[[tempIndex]]];


];


newRange = tempPoint[[2]] <=y<= currentPoint[[2]];


arrRange[[currentIndex]] = newRange;
currentIndex = tempIndex;
currentRange = calculateRange[tempPoint,currentIndex,arrRange,arrSlope];

AppendTo[rangeOrdered,newRange];
AppendTo[pointOrdered,tempPoint];

currentFun = arrFun[[currentIndex]];

currentPoint= tempPoint;
,
notIntersection = True;
AppendTo[rangeOrdered,-11<=y<=currentPoint[[2]]];
];
];

];
];

);


FindPoint[func_,yValue_]:= Return[ x/.Solve[func,{x},Reals][[1]]/.y->yValue]


(* creazione del piecewise utilizzando gli array generati dalla creazione del percorso *)
CreatePiecewise[funOrdered_,pointOrdered_,rangeOrdered_]:=(
Module[{tempPiecewise},
tempPiecewise = {};
For[i=1,i<=Length[funOrdered],i++, (* ciclo su tutte le rette del percorso e unisco i range e punti corrispondenti per la creazione *)
If[funOrdered[[i]]===0, (* 0 significa termine del percorso *)
AppendTo[tempPiecewise,{{pointOrdered[[Length[pointOrdered]]][[1]],pointOrdered[[Length[pointOrdered]]][[2]]+0.13},y<=(pointOrdered[[Length[pointOrdered]]][[2]])} ],
AppendTo[tempPiecewise,{{FindPoint[funOrdered[[i]],y],y+0.13},rangeOrdered[[i]]} ]
];
];
Return[Piecewise[tempPiecewise]];
]);


InitializeStar[arrStar_]:= (
Module[{star={}},

For[i=1,i<=Length[arrStar],i++,
AppendTo[star,{Green,PointSize[0.015],Point[arrStar[[i]]]}]
];
Return[star];
]);


CreatePoint[yInput_,pieceWise_,graphStar_,arrStar_,text_,result_,nLevel_]:=(
Module[{tempPoint={},tmpPieceWise},
tempPoint = pieceWise/.y->yInput;
For[i=1,i<=Length[graphStar],i++,
If[tempPoint[[1]]-0.400<=graphStar[[i]][[3]][[1]][[1]]<=tempPoint[[1]]+0.400 && tempPoint[[2]]-0.400<=graphStar[[i]][[3]][[1]][[2]]<=tempPoint[[2]]+0.400, 
graphStar[[i]][[3]] = Point[{-20,20}]; result=result+1;]; 
];
If[result===Length[arrStar[[nLevel]][[1]]], text= Text[Style["LEVEL COMPLETED",{0,0},FontSize->40]]];
If[yInput>=9.8, graphStar= InitializeStar[arrStar[[nLevel]][[1]]];result=0;];

Return[graphStar];
]);


CreateAnimate[funcPlot_,linePlot_,graphStar_,arrStar_,pieceWise_,nLevel_]:=(
SetAttributes[CreatePoint,HoldAll];
DynamicModule[{result=0, text={}},

Animate[Show[{
funcPlot,
Graphics[{
linePlot,
{PointSize[Large],Point[pieceWise/.y->yvalue]},
(*Punto da colpire,scompare (esce dall'area del grafo) se superato*)
CreatePoint[yvalue,pieceWise,graphStar,arrStar,text,result,nLevel],
text
(*text box che mostra la curva del grafico
LightGray,Rectangle[{9,-9.4},{11,-11}],Black,Text["f = " f[x],{10,-10}]*) }]},Axes->True,AxesOrigin->{0,0},ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}],
{yvalue,10,-11,.125},AnimationRunning->True,AnimationRepetitions->1,ControlPlacement->Bottom] (*,AnimationRate->2.5*)

]);


checkInput[a_,b_,c_,startRange_,endRange_]:=(
Module[{patternfunc,arrErrors},
patternfunc ="[+-]?(((\\d)+/[1-9](\\d)*)|(\\d)+)";
arrErrors = {};
If[a==="" && b==="",Return[{False,False,False}]];

If[StringMatchQ[a,RegularExpression[patternfunc]]===False, AppendTo[arrErrors,False],AppendTo[arrErrors,True]];
If[StringMatchQ[b,RegularExpression[patternfunc]]===False, AppendTo[arrErrors,False],AppendTo[arrErrors,True]];
If[StringMatchQ[c,RegularExpression[patternfunc]]===False, AppendTo[arrErrors,False],AppendTo[arrErrors,True]];

If[startRange=!="",
If[StringMatchQ[startRange,RegularExpression[patternfunc]]===False, AppendTo[arrErrors,False],AppendTo[arrErrors,True]],
AppendTo[arrErrors,True] ];
If[endRange=!="",
If[StringMatchQ[endRange,RegularExpression[patternfunc]]===False, AppendTo[arrErrors,False],AppendTo[arrErrors,True]],
AppendTo[arrErrors,True]];

If[arrErrors[[Length[arrErrors]]]===True &&arrErrors[[Length[arrErrors]-1]]===True,

If[ToExpression[startRange]>ToExpression[endRange],AppendTo[arrErrors,False]]];

Return[arrErrors]
]);


checkDoubleFun[fun_,arrFun_]:= (
For[i=1,i<=Length[arrFun],i++,
If[fun == arrFun[[i]],Return[True]]];
Return[False]);


addOnArr[a_,b_,c_,xy_,startRange_,endRange_,inputFun_,arrRange_,arrSlope_] :=(
Module[{arrErrors,tempA,tempB,tempC,tempFunc,tempStart,tempEnd,slope},
arrErrors = checkInput[a,b,c,startRange,endRange];
If[ContainsAny[arrErrors,{False}],
MessageDialog["Errore"];
Return[];
];
tempA = ToExpression[a];
tempB = ToExpression[b];
tempC = ToExpression[c];
xy = ToExpression[xy];

If[tempB===0 , tempFunc = x ==-tempC/tempA, 
tempFunc= y== -tempA/tempB x -tempC/tempB
  ];

If[Not[checkDoubleFun[tempFunc,inputFun]],
AppendTo[inputFun,tempFunc];

If[tempB==0,
slope=0,

slope = -tempA/tempB;

];
AppendTo[arrSlope,slope];

tempStart=-12;
tempEnd=12;
If[startRange=!="",tempStart = ToExpression[startRange]];
If[endRange =!= "",tempEnd = ToExpression[endRange]];
AppendTo[arrRange,tempStart<=xy<=tempEnd];
]
]);


findFun[fun_,arrFunc_]:=(
For[i=0, i<= Length[arrFunc],i++,
If[arrFunc[[i]] === fun,Return[i]]
]
);


deleteFun[inputFun_,arrRange_,arrSlope_,delfun_,popup_]:=(
Module[{indexdel},
indexdel = findFun[delfun,inputFun];
inputFun = Delete[inputFun,indexdel];
arrRange = Delete[arrRange,indexdel+1];
arrSlope = Delete[arrSlope,indexdel+1];
popup = PopupMenu[Dynamic[delfun],inputFun];

]);


CreatePlot[arrFunc_,arrRange_] := 
Module[{range1,range2,funcTemp,funcPlot={},linePlot={}},


For[indice=1,indice<=Length[arrFunc],indice++,
	If[ToString[arrFunc[[indice]][[1]]]=!="x",
		range1 = ToExpression[arrRange[[indice]][[1]]];
		
		range2 = ToExpression[arrRange[[indice]][[3]]]; 
		funcTemp = arrFunc[[indice]][[2]]; 

		AppendTo[funcPlot,Plot[Evaluate[funcTemp],{x,range1,range2}]],
		AppendTo[linePlot,{Blue,Line[{{arrFunc[[indice]][[2]],arrRange[[indice]][[1]]},{arrFunc[[indice]][[2]],arrRange[[indice]][[3]]}}]}]
];
];

Return[{funcPlot,linePlot}];

];


addFun[funca_,funcb_,funcc_,xy_,range1_,range2_,inputFun_,arrRange_,arrSlope_,graphStar_,delfun_,PlotTemp_,popup_,arrStar_,nLevel_]:=(
addOnArr[funca,funcb,funcc,xy,range1,range2,inputFun,arrRange,arrSlope];
res = CreatePlot[inputFun,Delete[arrRange,1]];
PlotTemp = Show[{res[[1]],Graphics[{res[[2]],graphStar,{PointSize[Large],Point[arrStar[[nLevel]][[2]]]}}]},AxesOrigin->{0,0},Axes->True,ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}];
popup = PopupMenu[Dynamic[delfun],inputFun]
);


startFun[inputFun_,funOrdered_,pointOrdered_,piecewise_,PlotTemp_,arrRange_,arrSlope_,
rangeOrdered_,graphStar_,arrStar_,nLevel_]:=(
Module[{ranges,slopes,arrFunc},
arrFunc = Insert[inputFun,x == arrStar[[nLevel]][[2]][[1]],1];
ranges = arrRange;
slopes = arrSlope;
funOrdered = {arrFunc[[1]]};
pointOrdered = {{arrFunc[[1]][[2]],10}};
rangeOrdered = {};
calculatePath[arrFunc,ranges,slopes,funOrdered,rangeOrdered,pointOrdered];


piecewise = CreatePiecewise[funOrdered,pointOrdered,rangeOrdered];

res = CreatePlot[inputFun,Delete[ranges,1]];

PlotTemp = CreateAnimate[res[[1]],res[[2]],graphStar,arrStar,piecewise,nLevel]
];)


CreateInitPlot[graphStar_,PlotTemp_,arrStar_,nLevel_]:=(
graphStar = InitializeStar[arrStar[[nLevel]][[1]]];

PlotTemp = Show[Graphics[{graphStar,{PointSize[Large],Point[arrStar[[nLevel]][[2]]]}}],Axes->True,AxesOrigin->{0,0},
ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}];
)


ResetData[inputFun_,result_,arrRange_,arrSlope_,funOrdered_,rangeOrdered_,pointOrdered_,popup_,delfun_]:= (
inputFun = {};
result = 0;
arrRange={-11<=x<=10};
arrSlope={0};
funOrdered={};
rangeOrdered={};
pointOrdered={};
delfun="";
popup = PopupMenu[Dynamic[delfun],inputFun]
)


(* ::InheritFromParent:: *)
(**)


init[]:=(

Print[DynamicModule[{funca= "",funcb="",funcc="",xy,range1="",range2="",
arrRange={-11<=x<=10},arrSlope={0},inputFun={},PlotTemp,popup,delfun="",
add,delete,start,enable=true,res,text={},result=0,available=False,
funOrdered={},rangeOrdered={}, pointOrdered={},piecewise,arrStar={
{{{-2,-2},{-4,-4},{-6,-6}},{0,10}}, (* x - y = 0, easy level*)
{{{2,0},{4,-1},{8,-3}},{0,10}}, (* x + 2y - 2  = 0 -> y = -.5*x +1, easy level*)
{{{0,3},{-1,2},{-6,-3}},{0,10}}, (* x - y + 3 = 0 fino a 2+, x - 2y = 0 dopo *)
{{{2,-1},{4, -2}, {7, -7}},{0,10}}, (* x + 2y = 0 per x tra 0 e [un numero tra 4 e 7], x + y = 0 da [un numero tra 4 e 7] in poi  *)
{{{-4,-1},{0, 0}, {-8, -2}},{0,10}}, (* x - 4y = 0 *)
{{{2,2},{3, 1}, {4, 0}},{0,10}}, (* x + y - 4 = 0 *)
{{{1,1},{2,3},{5,4.3}},{8,10}}, (*  per i primi 2 punti = 2*x -1*y -1 = 0 (x<2); per gli ultimi due punti (x>2) 13*x - 30*y + 64 = 0  *)
{{{0,5},{-2,3}, {-5,0}},{1,10}}, (* 1*x  - 1*y +5 = 0 *)
{{{0,5},{2,3}, {5,0}},{-1,10}}, (* -1*x -1*y +5 = 0 *)
{{{5,0},{0,-3}, {-6,-9}},{6,10}}, (* punti 1 e 2:  3*x -5*y - 15 = 0 ; punti 2 e 3: 1x - 1y -3 = 0*)
{{{5,5},{-5,0}, {0,-10}},{5,10}}}, (* upper: 1x -2y +5 = 0; lower : 9x + 5y + 10 = 0*)
graphStar, nLevel=1},


SetAttributes[addOnArr,HoldAll];
SetAttributes[ResetData,HoldAll];
SetAttributes[deleteFun,HoldAll];
SetAttributes[calculatePath,HoldAll];
SetAttributes[CreateAnimate,HoldAll];
SetAttributes[CreatePoint,HoldAll];
SetAttributes[addFun,HoldAll];
SetAttributes[startFun,HoldAll];
SetAttributes[CreateInitPlot,HoldAll];
SetAttributes[calculateRange,HoldAll];
SetAttributes[getValueRange,HoldAll];

(*
graphStar = InitializeStar[arrStar[[nLevel]][[1]]];

PlotTemp = Show[Graphics[{graphStar,{PointSize[Large],Point[{1,10}]}}],Axes->True,AxesOrigin->{0,0},
ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}];*)
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel];

popup = PopupMenu[Dynamic[delfun],inputFun];

Column[{Dynamic[PlotTemp],
Row[{Button["Previous",If[nLevel > 1, nLevel--;
ResetData[inputFun,result,arrRange,arrSlope,funOrdered,rangeOrdered,pointOrdered,popup,delfun];
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel];

]]
,
Button["Next",If[nLevel <  Length[arrStar], nLevel++;
ResetData[inputFun,result,arrRange,arrSlope,funOrdered,rangeOrdered,pointOrdered,popup,delfun];
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel];

]]}],
Panel[Column[
{Row[{

InputField[Dynamic[funca],String,FieldHint->"Enter a",FieldSize->Tiny]," x ",
InputField[Dynamic[funcb],String,FieldHint->"Enter b",FieldSize->Tiny], " y ",
InputField[Dynamic[funcc],String,FieldHint->"Enter c",FieldSize->Tiny]," =0 "}],
Row[{InputField[Dynamic[range1],String,FieldHint->"Enter range1",FieldSize->Tiny]," <= ",
PopupMenu[Dynamic[xy],{"x","y"}]," <= ",
InputField[Dynamic[range2],String,FieldHint->"Enter range2",FieldSize->Tiny]

}],

Row[{Dynamic[popup],
delete = Button["Delete",
deleteFun[inputFun,arrRange,arrSlope,delfun,popup];
If[Length[inputFun]>0,available=True,available=False];
res = CreatePlot[inputFun,Delete[arrRange,1]];
If[res[[1]] == {}, 
PlotTemp = Show[Graphics[{res[[2]],graphStar,{PointSize[Large],Point[arrStar[[nLevel]][[2]]]}}],Axes->True,AxesOrigin->{0,0},ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}],

PlotTemp = Show[{res[[1]],Graphics[{res[[2]],graphStar,{PointSize[Large],Point[arrStar[[nLevel]][[2]]]}}]},AxesOrigin->{0,0},ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}]
];
,
Enabled->Dynamic[available]

]
}]

}],"INSERISCI LA FUNZIONE"],

add = Button["Add",
addFun[funca,funcb,funcc,xy,range1,range2,inputFun,arrRange,arrSlope,graphStar,delfun,PlotTemp,popup,arrStar,nLevel];
If[Length[inputFun]>0,available=True,available=False];
],

start = Button["Start",
startFun[inputFun,funOrdered,pointOrdered,piecewise,PlotTemp,arrRange,arrSlope,
rangeOrdered,graphStar,arrStar,nLevel];
]

}]
]]);


End[ ]
EndPackage[ ]


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)
