(*Alpha Din Gabisi
Patient Manipulation*)
type patient = int * int * int * int *int list list*bool * int;
type casual = patient list; 
type listList = casual list;

(*Ambulance manipulation*)
type ambulance = int * int* bool * int * int; 
type ambL = ambulance list;

(*Random number generation*)
val seed = 
let
  val m=Date.minute(Date.fromTimeLocal(Time.now()))
  val s=Date.second(Date.fromTimeLocal(Time.now()))
  in 
	Random.rand(m,s)
end;

fun getRandInt(n)=Random.randRange(1,n) seed;
fun getRandInt1(n)=Random.randRange(1000,n) seed;
fun getRandInt2(n) = Random.randRange(0,n) seed;

(*This is used for generating delay in time steps*)
fun getRandTime(n) = Random.randRange(5, n) seed; 


fun subone(a) = a-1;
fun plusone(a) = a + 1;
(*this checks if a number is 1 or 0 then converts to a boolean value*)
fun checkOne(a:int) = if a = 1  
	then true else false;

(*This gets a list from an int list list*)
fun getlist (i:int list list, n) = List.nth(i,n);
(*this get a list from a casuality*)
fun getCas(b:casual, c:int) = List.nth(b,c);
(*this get a list from a list of list*)
fun getLL(b:listList, c:int) = List.nth(b,c); 
(*this will get a tuple of ambulances from a list*)
fun getAmb(b:ambL, c:int) = List.nth(b,c);

(*this generate the injuries based on number of injuries*)
fun genInjuries(b)= if (b > 0)
	then 
		let
			val injType = getRandInt(4);
			val sever = getRandInt(3);
			val c  = [injType, sever];
		in  [c]@genInjuries(subone(b)) end
	else [];

(*This will generate the survival k has to start at zero*)
fun genSurv(sur, c:int list list,numbI,k)= if (numbI > 0)
	then
		let 
			val firstL = getlist(c,k);
			val b = List.nth (firstL,1) ;
			val times = b * 10
			val getsur = sur;
			val sur = getsur - times;
		in genSurv(sur, c, subone(numbI),plusone(k)) end
	else 
		sur;

(*This function will generate ambulances*)
fun genAmb (numb:int, amb:ambulance, a:int) = if (numb = 1)
	then 
		let
			val amb = (a, 3,false,0,0 );
		in [amb] @ genAmb(subone(numb), amb,plusone(a)) end
	else if (numb > 1 andalso numb < 10 )
		then 
			let
				val amb = (a, 2,false, 0,0);
			in [amb] @ genAmb(subone(numb), amb, plusone(a)) end
	else if (numb >= 10)
		then 
			let 
				val amb = (a, 1,false,0,0);
			in [amb] @ genAmb(subone(numb), amb, plusone(a)) end
	else [];

(*This function will print all ambulances*)
fun printAmb(numb:int, k:ambL, b:int) = if (numb > 0)
then 
	let 
		val a = getAmb(k,b);
		val id = #1 a;
		val aType = #2 a;
		val dep = #3 a;
		val delay = #4 a;
		val tdelay = #5 a;
	in print("Ambulance ID:" ^ Int.toString id ^" Type: "^ Int.toString aType ^ " Deploy:" ^ 
				Bool.toString dep ^ " Delay Time Steps: " ^ Int.toString delay ^ " Delay to Treat: " ^
				Int.toString tdelay ^ "\n"); printAmb(subone(numb), k, plusone(b)) end
else print("These are all the ambulances\n") 


(*print all injuries *)
fun printInj(c:int list list,numbI,k) = if (numbI > 0)
	then 
		let 
			val a = getlist(c, 0); 
			val inj = List.nth(a,0); 
			val sever = List.nth(a,1);

		in print(" Injury type: " ^ Int.toString inj ^ " Severity: " ^ Int.toString sever); printInj(c,subone(numbI), plusone(k)) end
	else [];

(*print function*)
fun printPat (i:int, c:listList, b) = if (i > 0)
	then 
		let
			val cas = getLL(c,b);
			val k = getCas(cas,0);
			val id = #1 k; 
			val age = #2 k;			
			val survival = #3 k;
			val numbI = #4 k;	
			val injList = #5 k;	  
			val xR = #6 k;		
			val td = #7 k;		
		in print ("Patient ID: " ^ Int.toString  id ^" Age: " ^ Int.toString age ^ " Survival: " ^ Int.toString survival ^
			" Number of Injuries: " ^ Int.toString numbI  ^" X-ray req: "^ Bool.toString xR ^" Time to death mins: " ^ Int.toString td); printInj(injList,numbI,0); print("\n");  printPat(subone(i), c, plusone(b)) end
	else print("\nThese are all the patient\n");


(*This will generate all the patient*)
fun populate (i:int, c:casual, id:int) = if (i > 0)
	then
		let 
			val age = getRandInt(3);
			val survival = 100;
			val numbI = getRandInt(4);
			val inj = genInjuries(numbI);
			val check = checkOne(getRandInt2(1));
			val actSurv = genSurv(survival, inj, numbI,0);
			val td = (real actSurv / 100.0) * 1440.0;
			val k = (id, age, actSurv, numbI, inj,check,trunc td);
			val c = [k];
		in [c]@populate(subone(i), c, plusone(id)) end
	else [];

(*This will return the survival*)
fun getSur (i:int, c:listList) = if (i >= 0)
	then 
		let 
			val cas = getLL(c,i);
			val k = getCas(cas,0);
			val it = #3 k;
		in it + 0  end
	else 
		i;

(*This will return the numb of inj*)
fun getNumb (i:int, c:listList) = if (i>=0)
	then 
		let 
			val cas = getLL(c,i);
			val k = getCas(cas,0);
			val it = #4 k;
		in it  + 0 end
	else 
		i;


(*This will get all the severity and place them in a list*)
fun getWor(c:int list list,numbI,k) = if (numbI > 0)
	then 
		let 
			val a = getlist(c, 0);
			val sever = List.nth(a,1);
		in [sever]@getWor(c,subone(numbI), plusone(k)) end
	else [];

(*This will check the greatest from two numbers a in this case is new b is old*)
fun checNum(a,b) = if (a > b) 
then
	let
		val b = a;
	in b + 0 end
else 
	a; 

(*This will find the worst severity from a list b is the worst*)
fun findWor(sList:int list, numb, b,c:int) = if(numb > 0)
	then 
		let 
			val sever = List.nth(sList,c);
			val b = checNum(sever, b);
		in findWor(sList,subone(numb), b, plusone(c)) end
else 
	b;

(*This will delete a list based on index*)
fun delete (_,   nil) = nil
  | delete (0, _::xs) = xs
  | delete (i, x::xs) = x::delete(i-1,xs);

(*This function will remove a patient with survival 0*)
fun delD(i:int, c:listList) = if (getSur(i,c) = 0)
	then delete (i,c) else [];  
	
(* This function will go through the entire list 
fun remDead(i:int, c:listList, b) = if (i > 0)
then 
	let 
		val c = delD(b,c); 
	in remDead(subone(i),c,plusone(b)) end
else [];*)

(*This function will give you worst severity of one patient, 
b is the list, c is the numb of patient d starts at zero*)
fun getSev(patL:listList, c:int) = if(c >= 0)
then 
	let 
		val cas = getLL(patL,c); 
		val k = getCas(cas,0);
		val numbI = #4 k;
		val injList = #5 k;
		val d=0;
		val getSList = getWor(injList,numbI,d);
		val num = 0;
		val num2 = 0;
		(*val it1 = getSList;*)
		val t1 = List.nth(getSList,0);
		val worst = findWor(getSList, numbI,num,num2)
	in print("This is the first sev: " ^ Int.toString t1 ^"\nThe numbI is: "^ Int.toString numbI ^ "The worst is " ^Int.toString worst ^ "\n"); worst  end
else 
	 c; 	

(*This function will get which type of ambulance to deploy based on worst 
survival
fun getType(i:int) = if (i = 3) then 
let 
	val deptype = 3;
	in deptype + 0 end
else if(i =2) 
then 
	let
		val deptype = 2;
		in deptype + 0 end 
else if(i=1) 
then 
	let 
		val deptype = 1;
	in deptype +0 end
else
	i;*)

(*Check amb*)
fun checkAmb(amb:ambL, i:int) = if (i >= 0)
then 
	let 
		val d = getAmb(amb, i);
		val dep = #3 d;
	in dep end
else false;


(*This will check ambulance availabilty *)
fun availAmb(amb:ambL, i:int)= if (checkAmb(amb, i))
then true else false;

(*This will check ambulance type *)
fun availType(amb:ambL ,i:int) = if (i >= 0)
then
	let
		val d = getAmb(amb, i)
		val type1 = #2 d;
	in type1 end
else i;


(*This function will choose which ambulance to deploy based on type*)
fun chooseAmb(amb:ambL, ty:int, b:int) = if(availAmb(amb, b) = false andalso availType(amb, b) = ty)
then 
	let
		val d = getAmb(amb, b);
		val getId = #1 d;
	in print("This is the numb: "^Int.toString getId^"\n"); b +0 end
else if(availAmb(amb, b) = true andalso availType(amb, b) = ty)
then 
	let 
		val a =[];
	in print("This ambulance is not available so deploying second best\n"); b - 1 end
else 
	chooseAmb(amb, ty, plusone(b));

(*This function will deploy ambulances*)
fun depAmb(amb:ambL, i:int) = if (i >= 0)
then 
	let 
		val d = getAmb(amb, i);
		val amb = delete(i,amb);
		val id = #1 d;
		val type1 = #2 d;
		val c = true;
		val n = 30;
		val delayts = getRandTime(n);
		val delaytr = #5 d;
		val a:ambulance = (id, type1, c, delayts, delaytr);
	in amb @ [a] end
else
	[];

(*This will return each timesteps for one ambulance*)
fun getTimeSteps(amb:ambL, b) = if(b >= 0)
then 
	let 
		val d = getAmb(amb, b)
		val timesteps= #4 d;
	in timesteps + 0 end
else
	getTimeSteps(amb, plusone(b));

(*This function will decrement the timesteps for each patient*)
fun decTime(amb:ambL, b) = if(getTimeSteps(amb,b) > 0)
then 
	let 
		val d = getAmb(amb, b);
		val amb = delete(b,amb);
		val id = #1 d;
		val type1 = #2 d;
		val c = #3 d;
		val timeSteps = #4 d -1;
		val delaytr = #5 d;
		val a = (id,type1,c,timeSteps, delaytr)
	in print("Initial"); amb @[a] end
else
	[];

(*This function will go through every ambulance and decrement 
everytime steps except for the one*)
fun decevAmb(amb:ambL, notDec, numb, b) = if(numb > 0 andalso b <> notDec andalso getTimeSteps(amb,b) > 0)
then 
	let
		val amb = decTime(amb, b);
	in print("DebugstepOne " ^ Int.toString b); decevAmb(amb, notDec, subone(numb), plusone(b)) end
else if(b = notDec)
then 
	let
		val a =[];
	in print("DebugstepTwo"); decevAmb(amb, notDec, subone(numb), plusone(b)) end 
else if (getTimeSteps(amb,b) = 0)
then 
	let 
		val a = [];
	in decevAmb(amb, notDec, subone(numb), plusone(b))end
else
	amb; 

(*Event manager This should be the master function that will do the time steps 
need another function to do the other tasks (i is the number of patients in the list, 
c is just the patient list, b starts 0, )*)
fun evenMan(i:int, c:listList,b, amb:ambL) = if (getSur(b,c) > 0 andalso i > 0)
then 
	let 
		val worstsurvival = getSev(c, b);
		val getAmb = chooseAmb(amb:ambL, worstsurvival, 0);	(*The last value could be one*)
		val amb = depAmb(amb, getAmb); 
		(*val amb = decevAmb(amb, getAmb, 21, 0);*)
	in printAmb(21, amb,0); evenMan(subone(i), c, plusone(b), amb) end
else if (getSur(b,c) = 0 andalso i > 0)
	then
		let 
			val a=[];
		in evenMan(subone(i), c, plusone(b), amb) end
else
	[]; 
					
				 


(*This will be the main*)
(*Generating patient*)
val c = (1000,1,90,1,[[1,1]],false, 1296)
val k = [c];
val x = populate(80,k, 1000);
val b = getCas(k,0);
val num = 80;
val num2 = 0;

printPat(num, x,num2);


(*val d= getSur(1,x);
val e = getNumb(1,x);*)

(*This will generate the ambulances*)
val a = (0, 0,false, 0, 0);
val y = genAmb(21, a, 1);
printAmb(21,y,0);


(*Test even man*)
evenMan(80,x,0,y);

(*Test for ambulance function
val dep = depAmb(y,1);
printAmb(21,dep,0);*)