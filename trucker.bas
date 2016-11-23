10 REM Independent Trucker Simulation
20 RANDOMIZE
30 REM a = INT(RND(1) * 10 + 1): PRINT a: GOTO 30
70 DIM mt(2), mp(2, 25), mp$(2, 25), mr$(2, 25), zm(2, 25), d$(6), nt$(4)
85 PRINT "Independent Trucker Simulation"
90 nt$(1) = "First": nt$(2) = "Second": nt$(3) = "Third": nt$(4) = "Fourth"
92 ds$(0)= "Monday": ds$(1) = "Tuesday": ds$(2) = "Wednesday": ds$(3)="Thursday": ds$(4)="Friday": ds$(5)="Saturday": ds$(6)="Sunday"
300 INPUT "Do you want to see instructions"; z$
310 IF LEFT$(z$,1)="N" OR LEFT$(z$,1)="n" THEN GOTO 1000 
320 GOSUB 6000
1000 xc=0: mf=0: hl=2: hs=7: hr=0: GOSUB 2100
1010 PRINT " "
1020 PRINT "You are at the Los Angeles Trucking Terminal"
1030 PRINT "Three types of cargo are available:"
1040 PRINT "1: ORANGES (highest profit IF they don't spoil)"
1050 PRINT "2: FREIGHT FORWARDING (penalty for late delivery)"
1060 PRINT "3: U.S. MAIL (lowest rate but no hurry to arrive)"
1070 PRINT "The cargo is due in New York by 4 pm on Thursday"
1075 INPUT "Which type of cargo do you want"; ct
1080 IF ct < 1 OR ct > 3 THEN 1030
1090 INPUT "How many pounds will you carry (40000 is the LEGAL limit, 50000 is the MAX limit)"; wl
1100 IF wl < 25000 THEN PRINT "You can't make a living on half a load.": GOTO 1090
1105 IF wl > 50000 THEN PRINT "Your truck cannot handle "; wl; " pounds": GOTO 1090
1110 PRINT "They are loading your truck now."
1120 RESTORE
1140 FOR rt = 0 TO 2
1141  REM PRINT "rt: "; rt
1150  READ np
1151  REM PRINT "np: "; np
1152  READ mt(rt)
1153  REM PRINT "mt: "; mt(rt)
1160  FOR i = 1 TO np
1170  READ mp(rt, i)
1171  READ mp$(rt, i)
1172  READ mr$(rt, i)
1174  REM PRINT "np: ";np; " rt: "; rt; " i: "; i; " mr: ";mr$(rt, i)
1175  READ zm(rt, i)
1180  NEXT i
1181 NEXT rt
1190 tc=10: wf=190: np=1: ts=1: sl=55: xn=xn+1: xc=190
1210 PRINT wl;" pounds of cargo has filled your trailer!"
1220 hr=hr+1: GOSUB 2100: PRINT ""
1225 PRINT "You have nearly a full tank (cost of fuel: $ 190)": PRINT ""
1230 INPUT "Two of your tires are worn.  Do you want replacements";z$:
1240 IF LEFT$(z$,1)="N" OR LEFT$(z$,1)="n" THEN 1350
1250 PRINT "You have the following tire options"
1251 PRINT "1: NEW tire (costs $ 200)"
1252 PRINT "2: RETREAD tire (costs $ 100)"
1260 INPUT "Which type do you want";z
1261 IF z < 1 OR z > 2 THEN PRINT "Invalid tire option": GOTO 1230
1262 IF z = 1 THEN INPUT "How many new tires (1-3)"; t
1263 IF z = 2 THEN INPUT "How many retreads (1-2)"; t
1280 IF t = 3 AND (z = 1) THEN ts=2:t=2:xc=xc+200 
1290 IF t < 0 OR t > 2 THEN PRINT "Invalid tire amount": GOTO 1230
1300 IF t = 0 THEN 1350
1310 IF z = 2 THEN tc=tc-(3*t): xc=xc+(100*t): GOTO 1350
1320 IF z = 1 THEN tc=tc-(4*t): xc=xc+(200*t): GOTO 1350
1340 GOTO 1230
1350 PRINT "The following routes are open:"
1351 PRINT "1: Northern route. Miles: 2,710 - Weather: Bad - Law Enforcement: High"
1352 PRINT "2: Middle route. Miles: 2,850 - Weather: Fair - Weighing Stations: High"
1353 PRINT "3: Southern route: Miles: 3,120 - Weather: Sunny - Law Enforcement: Low"
1360 INPUT "Which route (1-3)";z
1370 IF z = 1 THEN rt=1: rh=4: GOTO 1600
1380 IF z = 2 THEN rt=0: rh=2: GOTO 1600
1390 IF z = 3 THEN rt=2: rh=1: GOTO 1600
1391 GOTO 1350
1400 REM *** Recurring Checks ***
1410 af=(sp^2) * cd * cr
1420 IF af>(RND(1)*1e7) THEN GOTO 4000: REM - Crash
1430 af=SQR(mf+100)*tc
1440 IF af>(rh*25000*RND(1)) THEN GOSUB 2600: REM Flat Tire
1450 IF sp > (sl-rh+10) THEN GOSUB 2300: REM - Speeding
1460 hr=hr+1: hl=hl+1
1470 IF sl<40 THEN sl=55
1480 t=ABS(55-sp): IF t>12 THEN t=12.5
1490 t1=sp/(4.5 - (0.2*t))
1500 wf=(wf-t1): IF wf < 0  THEN GOSUB 2500: REM - Out of Gas
1510 mf=mf+sp
1520 IF mf > mt(rt) THEN 5000: REM - End of Journey
1530 FOR i = 1 TO 250: NEXT i
1550 PRINT "": PRINT "====================": GOSUB 2100
1560 approx= INT(wf-5)+INT(RND(1) * 10 + 1): gauge=INT(approx/200 * 10)
1561 PRINT "Approximate FUEL:"; approx;
1565 PRINT "(";: FOR i = 1 TO gauge: PRINT "=";: NEXT i: FOR i = gauge TO 10: PRINT " ";: NEXT i: PRINT ")"
1568 PRINT "Speed: "; sp
1569 PRINT "Speed Limit: "; sl
1570 PRINT "Odometer: "; mf
1579 PRINT "Miles to Go: "; mt(rt)-mf; 
1580 PRINT "": PRINT ""
1590 REM *** Milepost ***
1600 IF mp(rt,np) <= mf THEN GOTO 3100
1604 PRINT ""
1605 PRINT "Cruising on ";mr$(rt, np)
1610 GOSUB 3000: PRINT "You are feeling:    "; cd$
1620 GOSUB 2800: PRINT "Current weather:    "; cr$
1630 ns = ns+1: IF ns > 3 THEN GOSUB 1700
1640 GOSUB 1680
1670 GOTO 1400
1680 REM ** Speed setter **
1681 PRINT "": PRINT "How fast do you wish to go (20 - "; INT(sl * 1.5); ")";
1682 INPUT "";sp
1683 IF sp < 20 OR sp > INT(sl * 1.5) THEN PRINT "Invalid speed.": GOTO 1681
1690 RETURN
1700 REM ** Truck stop **
1710 PRINT "": INPUT "Truck stop ahead.  Do you want to stop (y/n)";z$
1720 IF LEFT$(z$,1) = "N" OR LEFT$(z$,1)= "n" THEN ns=1:hl=hl+1:RETURN
1730 IF LEFT$(z$,1) <> "Y" AND LEFT$(z$,1) <> "y" THEN PRINT "Invalid input.": GOTO 1710
1740 t=85+INT(35*RND(1))
1745 PRINT "": PRINT "Truck stop -- FUEL"
1750 PRINT "Diesel fuel costs "; t; " cents a gallon."
1751 PRINT "Current fuel: "; wf; " /200 gallons. ("; INT(wf/2); "%)"
1752 PRINT "How many gallons do you want (max: "; INT(200-wf); ")";
1760 INPUT "";t1
1765 IF (t1 > 200 - wf) THEN PRINT "Too much fuel.": GOTO 1750
1770 IF t1>0 THEN PRINT "": PRINT "Pay $ "; (t*t1/100): xc=xc+(t*t1/100): wf=wf+t1
1780 PRINT "So far, you have spent $ "; xc
1790 REM ** Truck stop - Tires **
1800 IF ts > 0 THEN 1900
1810 t = 200 + INT(50*RND(1)): t1=100 + INT(70*RND(1))
1820 PRINT "": PRINT "Truck stop -- TIRES"
1821 PRINT "1: Get a NEW Tire ($ ";t;")"
1822 PRINT "2: Go and RETREAD Tire ($ ";t1;")"
1823 PRINT "3: Skip tires"
1830 INPUT "Choose tire option";z
1840 IF z < 1 OR z > 3 THEN PRINT "Invalid tire option.": GOTO 1820
1850 IF z = 3 THEN GOTO 1900
1860 IF z = 1 THEN xc=xc+t: ts=2: GOTO 1900
1870 IF z = 2 THEN xc=xc+t1: ts=1: GOTO 1900
1900 hr=hr+1: ns=0
1901 REM ** Truck stop - Sleep **
1910 PRINT "": PRINT "Truck stop -- SLEEP"
1911 PRINT "You are feeling: "; cd$
1912 GOSUB 2100
1920 INPUT "Do you want to get some sleep (y/n)";z$
1930 IF LEFT$(z$,1)="N" OR LEFT$(z$,1)="n" THEN GOTO 2020
1935 INPUT "How many hours of rest(1-10)";t
1940 IF t<1 OR t>10 THEN PRINT "Invalid sleep option.": GOTO 1910
1950 dh=hr-(24*INT(hr/24))
1960 hr=hr+t: IF ct = 1 THEN wf=wf-(7*t): REM ** oranges use up fuel **
1970 IF dh<21 OR dh>12 THEN GOTO 1980
1975 t=INT(t/2.6)
1976 PRINT "Thanks to the daytime noise, you only got ";t;" hours of real sleep."
1980 hs=hs+t: hl=hl/2
1990 IF t > 3 THEN hl=0
2010 GOSUB 2100: PRINT "Time to hit the road again."
2020 PRINT "Remember, the speed limit is ";sl;" mph"
2030 GOSUB 8900: RETURN
2100 REM *** Display date and time ***
2110 dh = hr+8
2120 dt=INT(dh/24):dh=dh-(24*dt)
2130 IF dt>6 THEN dt=dt-7: GOTO 2130
2140 dm$="am"
2150 IF dh=12 THEN dm$="noon": GOTO 2210
2160 IF dh>12 THEN dh=dh-12: dm$="pm"
2170 IF dh=0 THEN dh=12: dm$="midnight"
2210 PRINT "": PRINT "Day: ";ds$(dt):PRINT " Time: ";dh;" ";dm$
2230 RETURN
2300 REM ** Speeding **
2310 IF (sp-sl-5)^2 < (900*RND(1)) THEN RETURN
2320 GOSUB 8900: PRINT "Smokey is behind you with his lights on.  PULL OVER!"
2350 nt=nt+1: PRINT "See the justice of the peace for your "; nt$(nt);" offense"
2360 PRINT "Wait "; nt; " hours for your hearing"
2370 hr = hr+nt:hl=hl+nt
2380 IF nt > 3 THEN GOTO 2430
2390 t = nt*INT(RND(1) * 5 + 1):t1 = 5*(rt+nt*INT(RND(1) * 4 + 1))
2400 PRINT "Fine is $ ";t1
2401 PRINT "Plus $ ";t;" for each MPH over the limit."
2410 PRINT "Pay total: $";: PRINT dd$;t1+(t*(sp-sl)):xc=xc+(t1+(t1*(sp-sl)))
2420 GOSUB 8900: GOSUB 1680: RETURN
2430 PRINT "You are sentenced to 30 days in jail for reckless driving."
2440 PRINT ""
2450 PRINT "Your I.C.C. Driver's License is revoked!"
2460 GOTO 5500: REM ** Gameover **
2500 REM ** Out of Gas **
2505 GOSUB 8900
2510 t1=t1+wf: wf=0: sp=0
2520 t=(4.5 - (0.2 * t)) * t1: mf= mf+t
2530 PRINT "After "; t;" more miles, you ran out of fuel (dummy!)"
2540 PRINT "It costs $ 200 to get a barrel of diesel delivered."
2550 wf=55:t1=INT(RND(5) * 5 + 1):hr=hr+t1:xc=xc+200:hl=hl+t1
2560 PRINT "You also wasted "; t1; " hours by your carelessness"
2570 IF ct <> 1 THEN GOTO 2580
2575 cx = cx+INT(RND(3) * 3 + 1)
2576 PRINT "Sitting with the refrigeration unit off is damaging the oranges."
2580 GOSUB 8900: RETURN
2600 REM ** Flat tire **
2610 GOSUB 8900
2620 PRINT "You just blew a tire!"
2630 IF ts=0 THEN 2700
2640 tc=tc-(2*tss):ts=0
2645 t$="inside"
2650 t=INT(RND(1) * 2 + 1): IF t=1 THEN t$="outside"
2660 PRINT "It took "; t; " hours to change the "; t$; " tire.": hr=hr+t:hl=hl+t+1
2670 RETURN
2700 REM ** No spare tire **
2710 PRINT "Since your spare has already been used, you have to call a tow "
2715 PRINT "truck from town to deliver a new tire to you."
2720 PRINT "This service cost $ 400 and took 4 hours."
2730 hr=hr+4:hl=hl+4:xc=xc+400
2740 GOSUB 8900: RETURN
2800 REM ** Road conditions **
2810 af=(3000 + mf) * RND(1): ON (rt+1) GOTO 2870,2820,2910
2820 IF af<3300 AND cr<> 50 THEN GOTO 2960
2830 IF af>4800 THEN GOTO 2965
2840 IF af>4600 THEN GOTO 2970
2850 IF af>3800 THEN GOTO 2975
2860 GOTO 2985
2870 IF af<3400 AND cr<>50 THEN GOTO 2960
2880 IF af>4900 THEN GOTO 2965
2890 IF af>4700 THEN GOTO 2970
2900 IF af<=4200 THEN GOTO 2985
2905 IF RND(3)=1 THEN 2975
2906 GOTO 2980
2910 IF af<4000 AND cr<>50 THEN GOTO 2960
2920 IF af>5700 THEN GOTO 2965
2930 IF af > 5500 THEN GOTO 2970
2940 IF af>4400 THEN GOTO 2980
2950 GOTO 2985
2960 cr=1: cr$="CLEAR & DRY": RETURN
2965 cr=50: cr$="B-L-I-Z-Z-A-R-D !!": RETURN
2970 cr=10: cr$="FOG -- Limited visibility": RETURN
2975 cr=5: cr$="LIGHT SNOW": RETURN
2980 cr=5: cr$="RAIN": RETURN
2985 cr=3: cr$="CLEAR, but roadway is wet": RETURN
3000 REM ** Condition of Driver **
3010 IF hl>19 OR hr/hs>4 THEN cd=100: cd$= ".. E.X.H.A.U.S.T.E.D. ..": RETURN
3020 IF hl<3 AND hr * 1.0/hs < 2.3 THEN cd=1: cd$= "RESTED & RARING TO GO": RETURN
3030 IF hl<8 AND hr * 1.0/hs < 2.5 THEN cd=2: cd$= "FINE": RETURN
3040 IF hl<12 AND hr/hs <= 3 THEN cd=4: cd$= " B O R E D " : RETURN
3050 IF hl<16 AND hr/hs <= 3 THEN cd=8: cd$ = " T I R E D ": RETURN
3060 cd=25: cd$ = "FATIGUED ... You're getting sleepy": RETURN
3100 REM ** Milepost **
3110 PRINT "You have just passed "; mp$(rt, np)
3120 zh=zm(rt,np): sl=55
3130 ON INT(zh) GOSUB 3210,3310, 3360, 3410, 3500, 3710, 3860
3140 np=np+1: IF INT(zh)=8 THEN GOTO 5000
3145 GOTO 1600
3210 PRINT "Time zone changes -- Set clock ahead one hour"
3220 hr = hr+1: GOSUB 2100
3230 GOSUB 8900: RETURN
3310 REM ** Toll gate **
3315 t=INT(100*(zh-INT(zh)))
3320 PRINT "Stop!  Pay toll of $ "; t
3330 xc = xc+t
3340 GOSUB 8900: RETURN
3360 REM ** Construction **
3365 IF RND(1) < zh - INT(zh) THEN RETURN
3370 PRINT "Construction ahead!"
3380 PRINT "Slow down -- Speed Limit 35 mph": sl=35
3390 GOSUB 8900: RETURN
3410 REM ** Radar **
3415 IF RND(1) < zh-INT(zh) THEN RETURN
3420 t=sp+RND(5)-2
3430 PRINT "You were just clocked by RADAR at "; t; " mph"
3440 IF t <= sl+3 THEN PRINT "No ticket this time": RETURN
3450 GOSUB 2320: RETURN
3500 REM ** Weighing Station **
3505 IF zh=INT(zh) AND RND(1) >= 0.5 THEN RETURN
3510 IF RND(1) < zh-INT(zh) THEN RETURN
3520 PRINT "WEIGHING STATION OPEN -- TRUCKS MUST STOP"
3530 PRINT "Scale weighs truck with cargo, fuel, & driver: "
3540 t=19000 + wl + (7*wf) + (25*INT(RND(10) * 10 + 1))
3550 PRINT "Weight: "; t; " pounds"
3560 t=INT(t-60000)
3570 IF t<1 THEN PRINT " You're O.K.": GOSUB 8900: RETURN
3580 IF zh=5.00 THEN GOTO 3630
3590 t1=INT(RND(4) * 4 + 1) +2: PRINT " Overweight fine is $ 200 plus "; t1; " cents per pound."
3600 xc = xc + 200 + (t*t1)/100
3610 PRINT "Pay fine of "; 200+(t*t1)/100
3620 GOSUB 8900: RETURN
3630 REM ** Louisiana overweight **
3640 PRINT "You are not allowed to enter Louisiana with that load."
3650 PRINT "Take a 200 mile detour through Arkansas with 45 mph speed limit."
3660 sl=45: mr$(rt,12)="Arkansas County Roads"
3670 FOR i = 12 TO 25: mp(rt, i) = mp(rt, i) + 200: NEXT i
3680 mt(rt)=mt(rt)+200
3690 GOSUB 8900: RETURN
3710 REM ** Rockslide **
3715 IF RND(1) < zh-INT(zh) THEN RETURN
3720 t=INT(RND(6) * 6 + 1)
3730 PRINT "A ROCK SLIDE has blocked the Alleghany Tunnel entrance"
3740 PRINT " The highway department will have it cleared in "; t; " hours"
3750 hr=hr+t: IF ct=1 THEN wf=wf-(7*t): IF wf<= 1 THEN GOSUB 3820
3755 t1=0: hl=0
3760 IF t>1 THEN t1 = INT(t/2 + 0.5)
3770 IF t1 <= 3 AND t1 > 0 THEN hl=hl/2
3780 hs=hs+t1
3790 PRINT " While waiting, you got "; t1; " hours of sleep"
3800 GOSUB 2100: GOSUB 8900: RETURN
3820 PRINT "You ran out of gas while waiting": t=0: GOSUB 2540
3830 GOSUB 8900: RETURN
3860 IF ct>1 THEN RETURN
3870 IF RND(1) < zh-INT(zh) THEN RETURN
3880 PRINT "The trailer refrigeration unit has failed, endangering the cargo"
3890 PRINT " Reparis take 2 hours and cost $ 100"
3900 cx=cx+INT(RND(4) * 4 + 1): hr=hr+2: hl=hl+2: xc=xc+100
3910 GOSUB 2100
3920 GOSUB 8900: RETURN
4000 REM ** Crash **
4020 PRINT "C R A S H !"
4060 PRINT ""
4070 IF cd=100 OR (cd-25 AND sp<65) THEN PRINT "You fell asleep at the wheel": GOTO 4130
4080 IF cr=50 THEN PRINT "You drove into a snow-filled ditch": GOTO 4130
4090 IF cr=10 THEN PRINT "You rear-ended a pick-up with no tail lights": GOTO 4130
4100 IF sp>65 THEN PRINT "SPEED KILLS !": END
4110 IF cr>2 THEN PRINT "You hit a slick spot and skidded off the road.": GOTO 4130
4120 PRINT "A drunk driver rammed your rig.  TOUGH LUCK !"
4130 INPUT "Press to continue";a$
4140 PRINT "You lose your truck and your profits": PRINT ""
4150 INPUT "Do you want to start over";z$
4160 IF LEFT$(z$,1)="N" OR LEFT$(z$,1)="n" THEN END
4170 xp=0:GOTO 1000
5000 REM ** Finish line **
5010 FOR i = 1 TO 5 
5020 FOR j = 1 TO 60: PRINT "*";: NEXT j: PRINT "*"
5030 PRINT "********* WELCOME TO NEW YORK *********  "
5040 NEXT i
5100 GOSUB 8900: GOSUB 2100
5110 t=hr-INT(hr/24): IF t < 10 OR t >21 THEN GOTO 5140
5120 PRINT "The warehouse is closed for the night.  Come back tomorrow."
5130 t=24-t:hr=hr+t: GOSUB 2100
5140 PRINT "" : t=INT(hr/24): t1=hr-(24*t)
5150 PRINT "You completed the trip in "; t; " days "
5160 IF t1 > 1 THEN PRINT " and "; t1; " hours "
5170 PRINT "Trip expenses total: $ "; dc$;" "; xc
5175 IF t1 > 0 THEN t = t+1
5180 t1 = (85 * t) + 85: PRINT "Truck payments, insurance and taxes cost: $ "; t1
5190 xc = xc+t1: PRINT ""
5200 ON ct GOTO 5220, 5310, 5360
5220 REM ** Oranges **
5225 t1=(t-4)*INT(RND(3) * 3 + 1): IF t1>0 THEN cx = cx+t1 
5230 IF cx>6 THEN PRINT "Your oranges have spoiled.  Take them away!": xt=-50: GOTO 5400
5240 PRINT "Collect 6.5 cents per pound for good oranges."
5250 xt = 0.065 * wl: PRINT "Total for the load: $ "; xt
5260 IF cx < 1 THEN GOTO 5400
5270 PRINT "Part of the load is damaged.  Subtract "; (5*cx); "%"
5280 xt=xt-(xt*cx/20): PRINT "Net payment is $ "; xt
5290 GOTO 5400
5310 REM ** Freight **
5315 xt = 0.06 * wl: PRINT "Collect 5 cents per pound of freight."
5320 PRINT " Total for load: $ "; xt
5330 IF hr<90 THEN GOTO 5400
5340 cx = 2: PRINT "You're late!  Subtract 10% penalty.": GOTO 5280
5360 REM ** Mail **
5365 PRINT "Postmaster pays 4.75 cents per pound on delivery.": xt=0.0475*wl
5370 cx = 0: GOTO 5290
5400 PRINT "": xt=xt-xc: xp=xp+xt: IF xt < 0 THEN GOTO 5470
5410 PRINT "Your Net Profit this trip is $ "; xt
5420 IF xt > 1000 THEN PRINT " G O O D   W O R K  ! ! ! "
5430 IF xn > 1 THEN PRINT "Your Average Profit has been $ "; (xp/xn)
5440 IF xt<200 OR xp/xn<250 THEN PRINT "You would make more money washing the dishes!"
5450 PRINT "": INPUT "Do you want to make another trip";z$
5460 IF LEFT$(z$,1)="N" OR LEFT$(z$,1)="n" THEN END
5465 GOTO 1000
5470 PRINT "BAD TRIP ... You lost $ "; ABS(xt)
5480 IF xp>=0 THEN GOTO 5430
5490 PRINT "You are BANKRUPT!"
5500 REM ** Lose truck, end game **
5510 PRINT "Your rig has been repossessed." : END
6000 GOSUB 8900
6010 PRINT "This is a simulation of the problems facing a long haul truck driver."
7240 GOTO 1000
8900 REM ** Press to continue **
8910 PRINT "...";: INPUT "";a$: RETURN
8916 RETURN
9000 REM ** Data **
9030 DATA 21,2850
9040 DATA 90,BARSTOW,I-15 in California,7.80
9050 DATA 225,NEEDLES,I-40 in California,1
9060 DATA 440,FLAGSTAFF,I-40 in Arizona,3.65
9070 DATA 620,GALLUP,I-40 in Arizona,5.5
9080 DATA 760,ALBUQUERQUE,I-40 in New Mexico,3.35
9090 DATA 930,TUCUMCARI,I-40 in New Mexico,1
9100 DATA 1040,AMARILLO,I-40 in Texas,7.80
9110 DATA 1155,OKLAHOMA,I-40 in Texas,5.5
9120 DATA 1305,OKLAHOMA CITY,I-40 in Oklahoma,2.65
9130 DATA 1530,MISSOURI Border,Oklahoma Turnpike,2.40
9140 DATA 1815,ST. LOUIS,I-44 in Missouri,0
9150 DATA 1980,TERRE HAUTE,I-70 in Illinois,5.5
9160 DATA 2050,INDIANAPOLIS,I-70 in Indiana,0
9170 DATA 2115,OHIO Border,I-70 in Indiana,1
9180 DATA 2220,COLUMBUS,I-70 in Ohio,5.5
9190 DATA 2350,WHEELING West Virginia,I-70 in Ohio,4.25
9200 DATA 2410,NEW STANTON,I-70 in Pennsylvania,6.75
9210 DATA 2570,HARRISBURG,Pennsylvania Turnpike,3.75
9220 DATA 2760,NEW JERSEY Border,Pennsylvania Turnpike,2.95
9230 DATA 2840,HOLLAND TUNNEL,I-70 in New Jersey,2.40
9240 DATA 9999,NEW YORK,New York Streets,0
9255 DATA 18,2710
9260 DATA 90,BARSTOW,I-15 in California,7.80
9270 DATA 245,LAS VEGAS,I-15 in California,1
9280 DATA 365,UTAH BORDER,I-15 in Nevada,0
9290 DATA 500,end of Interstate,I-15 in Utah,3.20
9300 DATA 555,SALINA,US-89 in Utah,4.50
9310 DATA 760,GRAND JUNCTION,I-70 in Utah,5.40
9320 DATA 1010,DENVER,I-70 in Colorado,3.75
9330 DATA 1190,NEBRASKA Border,I-76 in Colorado,1
9340 DATA 1450,OMAHA,I-80 in Nebraska,5.50
9350 DATA 1590,DES MOINES,I-80 in Iowa,4.75
9360 DATA 1750,ILLINOIS Border,I-80 in Iowa,5.6
9370 DATA 1910,GARY,I-80 in Illinois,2.50
9380 DATA 2050,OHIO Border,Indiana Turnpike,2.45
9390 DATA 2215,CLEVELAND,Ohio Turnpike,2.80
9400 DATA 2280,PENNSYLVANIA Border,I-80 in Ohio,4.16
9410 DATA 2615,EAST STROUDSBERG,I-80 in Pennsylvania,3.33
9420 DATA 2675,WASHINGTON BRIDGE,I-80 in New Jersey,2.20
9430 DATA 9999,NEW YORK,New York City streets,0
9450 DATA 25,3120
9460 DATA 75,PALM SPRINGS,I-10 in California,0
9470 DATA 225,BLYTHE,I-10 in California,1
9480 DATA 375,PHOENIX,I-10 in Arizona,0
9490 DATA 495,TUCSON,I-10 in Arizona,7.9
9500 DATA 650,LORDSBURG,I-10 in Arizona,5.75
9510 DATA 795,EL PASO,I-10 in New Mexico,0
9520 DATA 965,PECOS,I-10 in Texas,1
9530 DATA 1080,ODESSA,I-20 in Texas,0
9540 DATA 1250,ABILENE,I-20 in Texas,3.80
9550 DATA 1439,DALLAS,I-20 in Texas,0
9560 DATA 1610,LOUISIANA Border,I-20 in Texas,5.00
9570 DATA 1785,VICKSBURG,I-20 in Louisiana,0
9580 DATA 1965,ALABAMA Border,I-20 in Mississippi,1
9590 DATA 2100,BIRMINGHAM,I-20 in Alabama,4.25
9600 DATA 2200,GEORGIA Border,I-20 in Alabama,0
9610 DATA 2255,ATLANTA,I-20 in Georgia,0
9620 DATA 2320,CAROLINA Border,I-85 in Georgia,5.75
9630 DATA 2565,GREENSBORO,I-85 in North Carolina,7.85
9640 DATA 2680,VIRGINIA Border,I-85 in North Carolina,7.85
9650 DATA 2775,RICHMOND,I-85 in Virginia,0
9660 DATA 2880,WASHINGTON D.C.,I-95 in Virginia,0
9670 DATA 2920,BALTIMORE,I-95 in Maryland,2.30
9680 DATA 2990,NEW JERSEY Border,I-95 in Delaware,2.25
9690 DATA 3110,HOLLAND TUNNEL,New Jersey Turnpike,2.40
9700 DATA 9999,NEW YORK,City Streets,0

