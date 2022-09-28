(* ::Package:: *)

BeginPackage["MarbleSlide`"]
yvalue;
y;
init::usage=" "

Begin["`Private`"]


(*calcola punti intersezione tra 2 rette*)
getIntersection[f1_,f2_,range1_:-11<=y<=11,range2_:-11<=y<=11]:= Return[Solve[{f1,f2,range1,range2},{x,y}]]


(*calcola punti di intersezione tra 1 retta e le restanti*)
intersectLines[fun_,funArr_,rangeArr_,arrSlope_,currentRange_,ytmp_]:=(
Module[{result,index,temp,block},
	result = {-11,-11}; (*valore se non esiste intersezione*)
	index=-1;
	block=False;
	
For[i=1,i<=Length[funArr],i++,
 If[fun=!=funArr[[i]],
	
    temp= getIntersection[fun,funArr[[i]],currentRange,rangeArr[[i]]];
    
	If[temp=!={},  (* caso se esiste un punto di intersezione *)
	temp = {x,y} /. temp[[1]];
	
	If[temp == result,
		If[arrSlope[[index]]*arrSlope[[i]] <= 0,
			block=True;
			, 
			If[Abs[arrSlope[[i]]]<Abs[arrSlope[[index]]],
				index=i;]
			]
			,
		If[temp[[2]]>result[[2]] && temp[[2]]<ytmp[[2]], (* controlliamo che il punto sia il pi\[UGrave] prossimo alla pallina *)
		result = temp;
		
		index = i;
		block=False;
		]
			];
	
	
	
]]
]; 
Return [{result,index,block}];
]);


(* prendere valore a destra o a sinistra del range *)
getValueRange[pos_,funIndex_,arrRange_]:=(
Module[{position},
position = pos;

If[pos == 2,position = 3];

Return[arrRange[[funIndex]][[position]]];
]);


(* calcolare l'equazione per un valore di x *)
findSolRoot[fun_, valx_]:=(
Return[fun[[2]]/.x->valx]
);


(* calcola il nuovo range della retta corrispondente al percorso *)
calculateRange[point_,funIndex_,arrRange_,arrSlope_]:=(

If[arrSlope[[funIndex]]=!= 0,
	If[arrSlope[[funIndex]]>0,
		Return[arrRange[[funIndex]][[1]]<=x<=point[[1]]];,
	Return[point[[1]]<=x<=arrRange[[funIndex]][[2]]];
	],	
	Return[-11<=x<=11]
];
);


(*Confronta lo slope di due rette per controllare se ci si trova in una situazione in cui l'inclinazione delle due rette blocca il percorso della pallina *)
CheckSlope[slope1_,slope2_]:=(
	Return[slope1*slope2 < 0 ||(slope1==0 && slope2==0) || slope1==0]; 
	 
)


(* algoritmo di calcolo percorso 
* risultato \[EGrave] una sequenza di rette seguite dal proprio range e punti di intersezione che 
* costituiscono il percorso (funOrdered, rangeOrdered, pointOrdered)
*)
calculatePath[arrFun_,arrRange_,arrSlope_,funOrdered_,rangeOrdered_,pointOrdered_]:=(
Module[{block=False,isFalling=True,notIntersection=False,(* flag per gestire le varie fasi del calcolo del percorso *)
tempPoint,tempIndex,res,newRange,tempX,tempBlock,
currentIndex=1,currentFun=arrFun[[1]],currentRange=arrRange[[1]],currentPoint={arrFun[[1]][[2]],10},newPoint}, (* currentPoint \[EGrave] il punto dove si trova la pallina *)


While[block===False && notIntersection===False ,(* l'algoritmo viene eseguito finch\[EGrave] la pallina non si blocca o non vengono trovati punti di intersezione *)
If[isFalling===False, (* caso in cui la pallina non sta cadendo per forza di gravit\[AGrave], ovvero parallelamente all'asse y *)
res = intersectLines[currentFun,arrFun,arrRange,arrSlope,currentRange,currentPoint]; (* risultato della ricerca di punti di interseazione *)

If[res[[1]]=!={-11,-11}, (* se esiste un punto di intersezione *)
tempPoint = res[[1]];(* punto intersezione *)
tempIndex = res[[2]]; (* indice nell'array della retta intersecata *)
tempBlock = res[[3]]; 

If[ Not[CheckSlope[arrSlope[[tempIndex]],arrSlope[[currentIndex]]]] && Not[tempBlock], (* controllo se le 2 rette non hanno inclinazione opposta, se vero blocco l'algoritmo *)

AppendTo[funOrdered,arrFun[[tempIndex]]];
,

AppendTo[funOrdered,0]; (* 0 significa termine del percorso *)
block =True;
];

AppendTo[pointOrdered,tempPoint];
newRange = tempPoint[[2]]<=y<=currentPoint[[2]]; (* nuovo range della retta in cui mi trovo *)
AppendTo[rangeOrdered,newRange];
currentRange= calculateRange[tempPoint,currentIndex,arrRange,arrSlope]; (* nuovo range della retta in cui mi trovo per x *)
currentFun = arrFun[[tempIndex]];
currentIndex = tempIndex;
currentPoint = tempPoint;

,

(* caso in cui non esiste un punto di inters *)
If[arrSlope[[currentIndex]]>0,(* controllo l'inclinazione della retta per valutare quale estremit\[AGrave] del range corrente ho bisogno per calcolarne il nuovo,
ovvero dal punto in cui si trova la pallina all'estremit\[AGrave] della retta *)


tempX = getValueRange[1,currentIndex,arrRange];

,
tempX = getValueRange[2,currentIndex,arrRange];

];


newPoint={tempX,findSolRoot[arrFun[[currentIndex]],tempX]}; (* punto di intersezione con la nuova retta di caduta all'estremit\[AGrave] *)
AppendTo[rangeOrdered,newPoint[[2]]<=y<=currentPoint[[2]]];

currentPoint = newPoint;
AppendTo[pointOrdered,currentPoint];
isFalling =True; (* al prossimo ciclo la pallina seguir\[AGrave] la nuova retta di caduta *)
AppendTo[funOrdered,x==tempX];

(* nuova retta di caduta, range e slope *)
currentFun = x==tempX;
AppendTo[arrFun,currentFun];
AppendTo[arrRange,-11<=x<=currentPoint[[1]]];
AppendTo[arrSlope,0];

currentIndex = Length[arrFun];
currentRange = -11<=x<=currentPoint[[1]];
];
,

(* caso in cui la pallina segue la retta di caduta (falling = true) *)
res= intersectLines[currentFun,arrFun,arrRange,arrSlope,currentRange,currentPoint];
If[res[[1]] =!={-11,-11},(* se esiste un punto di intersezione *)

tempPoint = res[[1]];(* punto intersezione *)
tempIndex = res[[2]]; (* indice nell'array della retta intersecata *)
tempBlock = res[[3]];

(* se hanno la stessa inclinazione ma perpendicolarmente con un angolo di 90 gradi blocco l'algoritmo *)
If[(CheckSlope[arrSlope[[tempIndex]],arrSlope[[currentIndex]]] && StringContainsQ[ToString[arrFun[[tempIndex]][[1]]],"y" ] ) || tempBlock == True,

AppendTo[funOrdered,0];
block = True;
,

(* caso negativo, continuo e imposto la retta che dovr\[AGrave] seguire la pallina *)
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

(* caso in cui la callina segue la retta di caduta e non incontra intersezioni, termino *)
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


(* Funzione che crea e restituisce un array di punti verdi con coordinate prese in input *)
InitializeStar[arrStar_]:= (
Module[{star={}},

For[i=1,i<=Length[arrStar],i++,
AppendTo[star,{Green,PointSize[0.015],Point[arrStar[[i]]]}]
];
Return[star];
]);


(* controlla se la pallina nera \[EGrave] entrata in contatto con le verdi, se positivo aggiunge un punto al punteggio *)
CreatePoint[yInput_,pieceWise_,graphStar_,arrStar_,text_,result_,nLevel_]:=(
Module[{tempPoint={},tmpPieceWise},

tempPoint = pieceWise/.y->yInput;

For[i=1,i<=Length[graphStar],i++,
If[tempPoint[[1]]-0.400<=graphStar[[i]][[3]][[1]][[1]]<=tempPoint[[1]]+0.400 && tempPoint[[2]]-0.400<=graphStar[[i]][[3]][[1]][[2]]<=tempPoint[[2]]+0.400, (* controllo del contatto*)
graphStar[[i]][[3]] = Point[{-20,20}]; result=result+1;]; 
];
If[result===Length[arrStar[[nLevel]][[1]]], text= Text[Style["LEVEL COMPLETED",{0,0},FontSize->40]]];
If[yInput>=9.8, graphStar= InitializeStar[arrStar[[nLevel]][[1]]];result=0;]; (* Se non avviene nessun contatto inserisco nell'array sempre le posizioni iniziali delle verdi*)

Return[graphStar];
]);


(* genera l'animazione utilizzando gli array calcolati dalla funzione createpath con le informazioni per il percorso *)
CreateAnimate[funcPlot_,linePlot_,graphStar_,arrStar_,pieceWise_,nLevel_]:=(
DynamicModule[{result=0, text={}},

Animate[Show[{
funcPlot,
Graphics[{
linePlot,
{PointSize[Large],Point[pieceWise/.y->yvalue]},(* calcola la posizione della pallina su y *)
(*Punto da colpire,scompare (esce dall'area del grafo) se superato*)
CreatePoint[yvalue,pieceWise,graphStar,arrStar,text,result,nLevel],
text 
}]},Axes->True,AxesOrigin->{0,0},ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}],
{yvalue,10,-11,.125},AnimationRunning->True,AnimationRepetitions->1,ControlPlacement->Bottom] (*,AnimationRate->2.5*)

]);


(*controlla che l'input inserito dall'utente sia del pattern voluto, ovvero frazione o intero con segno opzionale*)
checkInput[a_,b_,c_,startRange_,endRange_]:=(
Module[{patternfunc,arrErrors},
patternfunc ="[+-]?(((\\d)+/[1-9](\\d)*)|(\\d)+)"; (* pattern *)
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

Return[arrErrors];
]);


(*controllo se la funzione inserita dall'utente non \[EGrave] gi\[AGrave] presente *)
checkDoubleFun[fun_,arrFun_]:=(
For[i=1,i<=Length[arrFun],i++,
If[fun == arrFun[[i]],Return[True]]];
Return[False];
);


(* controlla l'input dell'utente e lo elabora prima di inserirlo nell'array delle rette *)
addOnArr[a_,b_,c_,startRange_,endRange_,inputFun_,arrRange_,arrSlope_] :=(
Module[{arrErrors,tempA,tempB,tempC,tempFunc,tempStart,tempEnd,tempRange,slope},

arrErrors = checkInput[a,b,c,startRange,endRange];
If[ContainsAny[arrErrors,{False}],
MessageDialog["Errore: Controlla i valori inseriti! Devono essere interi o frazioni nella forma (+-)a/b ."];
Return[];
];

tempA = ToExpression[a];
tempB = ToExpression[b];
tempC = ToExpression[c];

(* trasformo in funzioni lineari per y *)
If[tempB===0 , tempFunc = x ==-tempC/tempA, 
tempFunc= y== -tempA/tempB x -tempC/tempB
  ];

(*calcolo il range *)
tempStart=-12;
tempEnd=12;

If[startRange=!="",tempStart = ToExpression[startRange]];
If[endRange =!= "",tempEnd = ToExpression[endRange]];
tempRange = tempStart<=x<=tempEnd;

If[Not[checkDoubleFun[tempFunc,inputFun]],
AppendTo[inputFun,tempFunc];
AppendTo[arrRange,tempRange];

If[tempB==0,
slope=0,
slope = -tempA/tempB;
];
AppendTo[arrSlope,slope];
,
MessageDialog["Retta gi\[AGrave] inserita!"]
]
]);


(* restituisce indice retta nell'array*)
findFun[fun_,arrFunc_]:=(
For[i=0, i<= Length[arrFunc],i++,
If[arrFunc[[i]] === fun,Return[i]]
]
);


(* eliminazione retta dagli array, utilizzata dal bottone delete *)
deleteFun[inputFun_,arrRange_,arrSlope_,delfun_,popup_]:=(
Module[{indexdel},
indexdel = findFun[delfun,inputFun];
inputFun = Delete[inputFun,indexdel];
arrRange = Delete[arrRange,indexdel+1];
arrSlope = Delete[arrSlope,indexdel+1];
popup = PopupMenu[Dynamic[delfun],inputFun];

]);


(* genera un array di plot e linee attraverso l'input di rette e i relativi range*)
CreatePlot[arrFunc_,arrRange_] := 
Module[{range1,range2,funcTemp,funcPlot={},linePlot={}},


For[indice=1,indice<=Length[arrFunc],indice++,
	If[ToString[arrFunc[[indice]][[1]]]=!="x",
		range1 = ToExpression[arrRange[[indice]][[1]]];
		
		range2 = ToExpression[arrRange[[indice]][[3]]]; 
		funcTemp = arrFunc[[indice]][[2]]; 

		AppendTo[funcPlot,Plot[Evaluate[funcTemp],{x,range1,range2}]],
		(* rette parallele all'asse y non essendo valutabili per x sono elementi line *)
		AppendTo[linePlot,{Blue,Line[{{arrFunc[[indice]][[2]],arrRange[[indice]][[1]]},{arrFunc[[indice]][[2]],arrRange[[indice]][[3]]}}]}]
];
];

Return[{funcPlot,linePlot}];

];


(* richiama tutte le funzioni utili all'aggiunta di una nuova retta, utilizzata dal bottone add *)
addFun[funca_,funcb_,funcc_,range1_,range2_,inputFun_,arrRange_,arrSlope_,graphStar_,delfun_,PlotTemp_,popup_,arrStar_,nLevel_]:=(

addOnArr[funca,funcb,funcc,range1,range2,inputFun,arrRange,arrSlope];
res = CreatePlot[inputFun,Delete[arrRange,1]];
PlotTemp = Show[{res[[1]],Graphics[{res[[2]],graphStar,{PointSize[Large],Point[arrStar[[nLevel]][[2]]]}}]},AxesOrigin->{0,0},Axes->True,ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}];
popup = PopupMenu[Dynamic[delfun],inputFun];
);


(* racchiude tutti i passaggi e le chiamate di funzioni per la simulazione del gioco, ovvero il calcolo del percorso, la creazione del piecewise e dell'animazione  *)
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


(* genera il plot iniziale vuoto con la pallina nera in cima e le palline verdi (stars)*)
CreateInitPlot[graphStar_,PlotTemp_,arrStar_,nLevel_]:=(
graphStar = InitializeStar[arrStar[[nLevel]][[1]]];

PlotTemp = Show[Graphics[{graphStar,{PointSize[Large],Point[arrStar[[nLevel]][[2]]]}}],Axes->True,AxesOrigin->{0,0},
ImageSize->Large,AspectRatio->1,PlotRange->{{-11,11},{-11,11}}];
)


(* funzioni per rinizializzare tutti parametri e array usati, quando si cambia livello per esempio*)
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


(* main del programma, racchiude tutte i parametri utili e principali per il funzionamento, oltre all'interfaccia
* funca,funcb,funcc,range1,range2 sono le variabili dove vengono salvati i valori inseriti dall'utente
* arrRange, arrSlope, inputFun condividono lo stesso ordine delle rette
* add, delete, start bottoni dell'interfaccia
* popup per la scelta della retta da eliminare
* funordered, rangeordered, pointordered vettori inizialmente vuoti dove verranno poi salvati i valori utili dopo l'esecuzione di CreatePath
* arrStar contiene i vari livelli del gioco: un vettore a 2 dimensioni dove le colonne corrispondono a coordinate delle "stars" e coordinate del punto di partenza
* e le righe ai livelli.
* il resto sono variabili utili per la gestione
*)
init[]:=(

Print[DynamicModule[{funca= "",funcb="",funcc="",range1="",range2="",
arrRange={-11<=x<=10},arrSlope={0},inputFun={},PlotTemp,popup,delfun="",
add,delete,start,enable=True,res,text={},result=0,available=False,
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

(* creazione del plot iniziale *)
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel];

popup = PopupMenu[Dynamic[delfun],inputFun];

(* interfaccia con plot, menu di input e bottoni per le azioni *)
Column[{Dynamic[PlotTemp],
Row[
(* bottone per tornare al livello precedente *)
{Button["Previous",If[nLevel > 1, nLevel--;
ResetData[inputFun,result,arrRange,arrSlope,funOrdered,rangeOrdered,pointOrdered,popup,delfun];
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel];

]]
,
(* bottone per andare al livello successivo *)
Button["Next",If[nLevel <  Length[arrStar], nLevel++;
ResetData[inputFun,result,arrRange,arrSlope,funOrdered,rangeOrdered,pointOrdered,popup,delfun];
CreateInitPlot[graphStar,PlotTemp,arrStar,nLevel];

]]}],
(* campi per inserimento dei valori delle rette e il range *)
Panel[Column[
{Row[{

InputField[Dynamic[funca],String,FieldHint->"Enter a",FieldSize->Tiny]," x ",
InputField[Dynamic[funcb],String,FieldHint->"Enter b",FieldSize->Tiny], " y ",
InputField[Dynamic[funcc],String,FieldHint->"Enter c",FieldSize->Tiny]," =0 "}],
Row[{InputField[Dynamic[range1],String,FieldHint->"Enter range1",FieldSize->Tiny]," <= ",
" x "," <= ",
InputField[Dynamic[range2],String,FieldHint->"Enter range2",FieldSize->Tiny]

}],

(* bottone di scelta tramite il popup e cancellazione di una retta *)
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
}],
Row[{(* bottone di aggiunta di una retta *)
add = Button["Add",
addFun[funca,funcb,funcc,range1,range2,inputFun,arrRange,arrSlope,graphStar,delfun,PlotTemp,popup,arrStar,nLevel];
If[Length[inputFun]>0,available=True,available=False];
],

(* bottone per calcolare il percorso e iniziare la simulazione del gioco dopo l'inserimento delle rette *)
start = Button["Start",
startFun[inputFun,funOrdered,pointOrdered,piecewise,PlotTemp,arrRange,arrSlope,
rangeOrdered,graphStar,arrStar,nLevel];
]
}]

}],"INSERISCI LA FUNZIONE"]



}]
]]);


End[ ]
EndPackage[ ]


(* ::InheritFromParent:: *)
(**)


(* ::InheritFromParent:: *)
(**)
