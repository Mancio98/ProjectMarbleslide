(* ::Package:: *)

BeginPackage["MarbleSlide`"]
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
    Print["cacco",temp];
    
	If[temp=!={}, 
	temp = {x,y} /. temp[[1]];
	Print[temp];
	If[temp[[2]]>result[[2]] && temp[[2]]<ytmp[[2]],
		result = temp;
		index = i;
		];
	If[temp == result,
		If[arrSlope[[index]]*arrSlope[[i]] <= 0,
			block=True;
			, 
			If[Abs[arrSlope[[i]]]<Abs[arrSlope[[index]]],
				index=i;]
			]
			]
	] 
]
]; 
Return [{result,index,block}];
]);


getValueRange[pos_,funIndex_,arrRange_]:=(
Module[{position},
position = pos;

Print["ArrRange: ", arrRange];
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
	Return[slope1*slope2>0 || (slope1==0 && slope2==0)]; 
	 
)


calculatePath[arrFun_,arrRange_,arrSlope_,funOrdered_,rangeOrdered_,pointOrdered_]:=(
Module[{block=False,isFalling=True,notIntersection=False,
tempPoint,tempIndex,res,newRange,tempX,
currentIndex=1,currentFun=arrFun[[1]],currentRange=arrRange[[1]],currentPoint={arrFun[[1]][[2]],10},newPoint,tempRes},

SetAttributes[calculateRange,HoldAll];
SetAttributes[getValueRange,HoldAll];

While[block===False && notIntersection===False ,
If[isFalling===False,
res = intersectLines[currentFun,arrFun,arrRange,arrSlope,currentRange,currentPoint];
Print["intersect",res];

If[res[[1]]=!={-11,-11},
tempPoint = res[[1]];
tempIndex = res[[2]];

If[res[[3]]==False && CheckSlope[arrSlope[[tempIndex]],arrSlope[[currentIndex]]],

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
Print["intersect falling ",res];
If[res[[1]] =!={-11,-11},

tempPoint = res[[1]];
tempIndex = res[[2]];
tempRes = res[[3]];

Print["ArrFun: ", arrFun[[tempIndex]]];
Print["ArrFunCurrent: ", arrFun[[currentIndex]]]; 

If[!CheckSlope[arrSlope[[tempIndex]],arrSlope[[currentIndex]]] && tempRes==True && ToString[arrFun[[tempIndex]][[1]]] === "y",

AppendTo[funOrdered,0];
block = True
,

isFalling = False;
AppendTo[funOrdered,arrFun[[tempIndex]]];
,

];


newRange = tempPoint[[2]] <=y<= currentPoint[[2]];


arrRange[[currentIndex]] = newRange;
currentIndex = tempIndex;
currentRange = calculateRange[tempPoint,currentIndex,arrRange,arrSlope];
Print["crange ",currentRange];

AppendTo[rangeOrdered,newRange];
AppendTo[pointOrdered,tempPoint];

currentFun = arrFun[[currentIndex]];

currentPoint= tempPoint;
,
notIntersection = True;
AppendTo[rangeOrdered,-11<=y<=currentPoint[[2]]];
];
];

Print["ArrRange: ", arrRange];
Print["ArrFun: ", arrFun];

];
];

);


FindPoint[func_,yValue_]:= Return[ x/.Solve[func,{x},Reals][[1]]/.y->yValue]


CreatePiecewise[funOrdered_,pointOrdered_,rangeOrdered_]:=(
Module[{tempPiecewise},
tempPiecewise = {};
For[i=1,i<=Length[funOrdered],i++,
If[funOrdered[[i]]===0, AppendTo[tempPiecewise,{{pointOrdered[[Length[pointOrdered]]][[1]],pointOrdered[[Length[pointOrdered]]][[2]]+0.13},y<=(pointOrdered[[Length[pointOrdered]]][[2]])} ],
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
graphStar[[i]][[3]] = Point[{-20,20}]; result= result+1;]; 
];
If[result===Length[arrStar], text= Text[Style["LEVEL COMPLETED",{0,0},FontSize->40]]];
If[yInput>=9.8, graphStar= InitializeStar[arrStar[[nLevel]][[1]]]];

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
text,
(*text box che mostra la curva del grafico*)
LightGray,Rectangle[{9,-9.4},{11,-11}],Black,Text["f = " f[x],{10,-10}] }]},Axes->True,AxesOrigin->{0,0},ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}],
{yvalue,10,-11,.125},AnimationRate->2.5,AnimationRunning->True,AnimationRepetitions->1,ControlPlacement->Bottom]

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


addOnArr[a_,b_,c_,xy_,startRange_,endRange_,arrFunc_,arrRange_,arrSlope_] :=(
Module[{arrErrors,tempA,tempB,tempC,tempFunc,tempStart,tempEnd,slope},
arrErrors = checkInput[a,b,c,startRange,endRange];
If[ContainsAny[arrErrors,{False}],
(*CAPIRE COSA FARE IN CASO DI ERRORE*)
Print["ERRORE"];
Return[arrErrors]
];
tempA = ToExpression[a];
tempB = ToExpression[b];
tempC = ToExpression[c];
xy = ToExpression[xy];

If[tempB===0 , tempFunc = x ==-tempC/tempA, 
tempFunc= y== -tempA/tempB x -tempC/tempB
  ];

AppendTo[arrFunc,tempFunc];

If[tempB==0,
slope=0,

slope = -tempA/tempB;
AppendTo[arrSlope,slope]

];
Print["SLOPE: ",slope];

tempStart=-11;
tempEnd=11;
If[startRange=!="",tempStart = ToExpression[startRange]];
If[endRange =!= "",tempEnd = ToExpression[endRange]];
AppendTo[arrRange,tempStart<=xy<=tempEnd];

]);


findFun[fun_,arrFunc_]:=(
For[i=0, i<= Length[arrFunc],i++,
If[arrFunc[[i]] === fun,Return[i]]
]
);


deleteFun[arrFunc_,arrRange_,delfun_,popup_]:=(
Module[{indexdel},
indexdel = findFun[delfun,arrFunc];
arrFunc = Delete[arrFunc,indexdel];
arrRange = Delete[arrRange,indexdel];
popup = PopupMenu[Dynamic[delfun],Delete[arrFunc,1]]
]);


CreatePlot[arrFunc_,arrRange_] := 
Module[{range1,range2,funcTemp,funcPlot={},linePlot={}},


For[indice=1,indice<=Length[arrFunc],indice++,
	If[ToString[arrFunc[[indice]][[1]]]=!="x",
		range1 = ToExpression[arrRange[[indice]][[1]]];
		
		range2 = ToExpression[arrRange[[indice]][[3]]]; 
		funcTemp = arrFunc[[indice]][[2]]; 

		AppendTo[funcPlot,Plot[Evaluate[funcTemp],{x,range1,range2}]],
		AppendTo[linePlot,{Red,Line[{{arrFunc[[indice]][[2]],arrRange[[indice]][[1]]},{arrFunc[[indice]][[2]],arrRange[[indice]][[3]]}}]}]
];
];

Return[{funcPlot,linePlot}];

];


addFun[funca_,funcb_,funcc_,xy_,range1_,range2_,arrFunc_,arrRange_,arrSlope_,graphStar_,delfun_,PlotTemp_,popup_]:=(
addOnArr[funca,funcb,funcc,xy,range1,range2,arrFunc,arrRange,arrSlope];
res = CreatePlot[Delete[arrFunc,1],Delete[arrRange,1]];
PlotTemp = Show[{res[[1]],Graphics[{res[[2]],graphStar,{PointSize[Large],Point[{1,10}]}}]},AxesOrigin->{0,0},ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}];
popup = PopupMenu[Dynamic[delfun],Delete[arrFunc,1]]
);


startFun[inputFun_,inputRange_,funOrdered_,pointOrdered_,piecewise_,PlotTemp_,arrFunc_,arrRange_,arrSlope_,
rangeOrdered_,graphStar_,arrStar_,nLevel_]:=(

inputFun=Delete[arrFunc,1];
inputRange=Delete[arrRange,1];
funOrdered = {arrFunc[[1]]};
pointOrdered = {{arrFunc[[1]][[2]],10}};
calculatePath[arrFunc,arrRange,arrSlope,funOrdered,rangeOrdered,pointOrdered];


piecewise = CreatePiecewise[funOrdered,pointOrdered,rangeOrdered];

Print[piecewise];
res = CreatePlot[inputFun,inputRange];

Print["funcordered", funOrdered];
Print["rangeordered", rangeOrdered];
Print["pointordered", pointOrdered];
PlotTemp = CreateAnimate[res[[1]],res[[2]],graphStar,arrStar,piecewise,nLevel]
)


CreateInitPlot[graphStar_,PlotTemp_,arrStar_,nLevel_]:=(
graphStar = InitializeStar[arrStar[[nLevel]][[1]]];

PlotTemp = Show[Graphics[{graphStar,{PointSize[Large],Point[arrStar[[nLevel]][[2]]]}}],Axes->True,AxesOrigin->{0,0},
ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}];
)


(*End[]*)


(* ::InheritFromParent:: *)
(**)


init[]:=(

Print[DynamicModule[{funca= "",funcb="",funcc="",xy,range1="",range2="",arrFunc={},
arrRange={-11<=x<=10},arrSlope={0},inputFun,inputRange,PlotTemp,popup,delfun="",
add,delete,start,enable=True,res,text={},result=0,
funOrdered={},rangeOrdered={}, pointOrdered={},piecewise,arrStar={{{{1,1},{2,3},{5,4.3}},{0,10}},
{{{0,5},{-2,3}, {-5,0}},{0,10}},
{{{0,5},{2,3}, {5,0}},{0,10}},
{{{5,0},{0,-3}, {-6,-9}},{0,10}},
{{{5,5},{-5,0}, {0,-10}},{-5,10}}},
graphStar, nLevel=1},


SetAttributes[addOnArr,HoldAll];
SetAttributes[deleteFun,HoldAll];
SetAttributes[calculatePath,HoldAll];
SetAttributes[CreateAnimate,HoldAll];
SetAttributes[CreatePoint,HoldAll];
SetAttributes[addFun,HoldAll];
SetAttributes[startFun,HoldAll];
SetAttributes[CreateInitPlot,HoldAll];
arrFunc = {x == arrStar[[nLevel]][[2]][[1]]};

(*
graphStar = InitializeStar[arrStar[[nLevel]][[1]]];

PlotTemp = Show[Graphics[{graphStar,{PointSize[Large],Point[{1,10}]}}],Axes->True,AxesOrigin->{0,0},
ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}];*)
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel];

popup = PopupMenu[Dynamic[delfun],Delete[arrFunc,1]];

Column[{Dynamic[PlotTemp],
Row[{Button["Previos",If[nLevel > 1, nLevel--;
arrFunc = {x == arrStar[[nLevel]][[2]][[1]]};
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel]]],
Button["Next",If[nLevel < 5, nLevel++;
arrFunc = {x == arrStar[[nLevel]][[2]][[1]]};
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel];
]]}],
Panel[Column[
{Row[{

InputField[Dynamic[funca],String,FieldHint->"Enter a",FieldSize->Tiny]," x ",
InputField[Dynamic[funcb],String,FieldHint->"Enter b",FieldSize->Tiny], " y ",
InputField[Dynamic[funcc],String,FieldHint->"Enter c",FieldSize->Tiny]," =0 "}],
Row[{InputField[Dynamic[range1],String,FieldHint->"Enter range1",FieldSize->Tiny]," <= ",
PopupMenu[Dynamic[xy],{x,y}]," <= ",
InputField[Dynamic[range2],String,FieldHint->"Enter range2",FieldSize->Tiny]

}],

Row[{Dynamic[popup],
delete = Button["Delete",
deleteFun[arrFunc,arrRange,delfun,popup];
res = CreatePlot[Delete[arrFunc,1],Delete[arrRange,1]];
If[res[[1]] == {}, 
PlotTemp = Show[Graphics[{res[[2]],graphStar,{PointSize[Large],Point[{1,10}]}}],Axes->True,AxesOrigin->{0,0},ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}],

PlotTemp = Show[{res[[1]],Graphics[{res[[2]],graphStar,{PointSize[Large],Point[{1,10}]}}]},AxesOrigin->{0,0},ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}]
];

]
}]

}],"INSERISCI LA FUNZIONE"],

add = Button["Add",
addFun[funca,funcb,funcc,xy,range1,range2,arrFunc,arrRange,arrSlope,graphStar,delfun,PlotTemp,popup]

],
start = Button["Start",
startFun[inputFun,inputRange,funOrdered,pointOrdered,piecewise,PlotTemp,arrFunc,arrRange,arrSlope,
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
