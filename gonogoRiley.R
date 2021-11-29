# gonogo.R 04/23/2020

#INDEX 1 through 35, XComm.R (35 functions)

gonogo=function(mlo=0,mhi=0,sg=0,newz=T,reso=0,ln=F,test=1,term1=T,BL=NULL,Y=NULL,X=NULL)
{
	lY=length(Y); lBL=length(BL); en12=rep(0,2);
	jvec=dud=lev=NULL;
	if(mlo == 0 & mhi == 0 & sg == 0 & newz == T) 
	{
	cat("Minimal entries to gonogo are: (1) mlo, mhi, sg; or (2) newz=F.\nTry again.\n\n");
	return();
	} else {	n2n3=0; endi=0;	sgrem=sg; 	g=", "; }
	if(!newz) {test=z$test; about = z$about; init=z$init; mlo=init[1]; mhi=init[2]; sg=init[3];}

	if(!is.element(test,1:4)) 	{vv="test must be 1,2,3 or 4. Try again\n"; cat(vv); return();}
	if(mhi < mlo) 			{vv="mhi-mlo must be nonnegative. Try again.\n"; cat(vv); return(); }
	if(mlo == mhi & sg > 0) test=3;
	if(mlo < mhi & sg == 0) test=4;
	if(test < 4) {if(sg <= 0) 	{vv="sg must be positive. Try again.\n"; cat(vv); return(); }}

	if(newz)
	{
	if(test == 1) blrb1();
	if(test == 2) blrb2();
	if(test == 3) blrb3();
	if(test == 4) blrb4();
	}

	if(test == 1 | test == 2)
	{
	del5=(mhi-mlo)/6;
	epsi=del5/1000;
	if(sg>(mhi-mlo)/6+epsi){cat(paste("sg is too big (sg <= ",round(del5,4),")\nTry again\n\n",sep="")); return();}
	}

	xx="Enter title (without quotes): "; yy="Enter units (without quotes): ";
	if(newz) {dat0=data.frame(numeric(0)); titl=readline(xx); unit=readline(yy); n1=n2=n3=p=lam=0; 
	en=c(n1,n2,n3); about=wabout(c(mlo,mhi,sg,n1,n2,n3,p,lam,reso)); savinit=c(mlo,mhi,sg);} else 
	{dat0=z$d0; about=z$about; titl=z$title; unit=z$units; en=z$en; n1=en[1]; n2=en[2]; n3=en[3]; 
	p=z$p; reso=z$reso; n2n3=z$n2n3; ln=z$ln; init=z$init; mlo=init[1]; mhi=init[2]; sg=init[3];
	lam=z$lam; test=z$test; savinit=z$savinit; term1=z$term1;}

	ttl0=titl; ttl1=ttl2=NULL;

	if(ln & newz){
	if(test < 3) { u=fgs(mlo,mhi,sg); mlo=u[1]; mhi=u[2]; sg=u[3]; }
	if(test == 3) {
	vv="For ln=T Bruceton, mlo (same as mhi) must be positive.\n";
	vv1="Also, sg must be between 0 and mlo/3. Try again.\n";
	if(mlo <= 0 | sg <= 0 | sg >= mlo/3) {cat(vv); cat(vv1); return();} else
	{mlo=log(mlo); mhi=log(mhi); sg=log(1+sg/mlo);}
	}
	if(test == 4) {
	vv="For ln=T Langlie, mlo < mhi and both must be positive. Try again.\n";
	if(mlo <= 0 | mhi <= 0 | mlo >= mhi) {cat(vv); return();} else {mlo=log(mlo); mhi=log(mhi);}
	}
	}

	if(!newz & test > 2) {BL=z$BL; dud=z$dud; lev=z$lev}

	if(newz & test < 3) cat("\n");
	
	xx="Enter BL (nRev and two i's (one 0 is OK): "; 
	
	if(ln) h2="Log " else h2="";
	if(test == 1 & newz) titl=paste(h2,"3pod: ",titl,sep="");
	if(test == 2 & newz) titl=paste(h2,"Neyer: ",titl,sep="");

	if(newz & test > 2) 
	{
		if(lBL == 0)
		{
		blrb6();
		xx=readline(xx); 
		xx=strsplit(xx," ",fixed=T); 
		BL=as.numeric(xx[[1]]);
		} 

	lBL=length(BL);
	if(lBL != 3){cat("3 integers are required.\nTry again\n\n"); return();}

	tx=paste(BL,collapse="");
	if(all(BL[-1] == c(0,1)) | all(BL[-1] == c(1,1))) BL[2:3]=c(1,0);
	I=BL[-1]; 
	a=prtrans(I); dud=a$dud; lev=a$lev;
	pm=c(1,-1); iz=which(I==0); lz=length(iz); 
	if(lz == 1) I=I[-iz]; 
	zp=zpfun(I); 
	if(lz == 0)zp=c(0,1)+pm*zp;
	if(lz == 1){if(iz == 1) zp=1-zp;} 
	pchar=paste(xlead0(zp,4),collapse=",");
	
	cat(dud); cat("\n\n");
	t3=paste(BL,collapse="");
	if(test == 3) titl=substitute(paste(h2,L[pchar]," ",Bruc[t3],": ",titl,sep=""));
	if(test == 4) titl=substitute(paste(h2,L[pchar]," ",Lang[t3],": ",titl,sep=""));
	ttl1=substitute(paste(L[pchar],sep=""));
	if(test == 3) ttl2=substitute(paste(Bruc[t3],sep="")) else 
	ttl2=substitute(paste(Lang[t3],sep=""));
	}
	
	init=c(mlo,mhi,sg)
	if(!newz) n2n3=prd0(z);
	
	# n1 is the random test quantity eventually required to complete Phase I

if(lY == 0)
{
#------------------------------------------------------------------------------------

	if(test == 1)
{
	if(endi == 0)
	{w=phaseI1(dat0,mlo,mhi,sg,reso,about,titl,unit,ln); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]];}	

	if(endi == 0)
	{w=phaseI2(d0,dat0,sg,reso,about,titl,unit,ln,term1); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]]; }
	en12[1]=nrow(d0);
	
	if(endi == 0)
	{w=phaseI3(d0,dat0,sg,reso,about,titl,unit,ln,term1);
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; }
	n1=nrow(d0);
	en12[2]=nrow(d0)-en12[1];

} 
	if(test == 2)
{
	if(endi == 0)
	{w=nphaseI(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,term1); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]];}	
	n1=en12[1]=nrow(d0); 
}
	if(test == 3)
{
	if(endi == 0)
	{w=bphaseI(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,BL); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; en12=w[[4]];}	
	n1=nrow(d0);
}

	if(test == 4)
{
	if(endi == 0)
	{w=lphaseI(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,BL); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; en12=w[[4]];}	
	n1=nrow(d0);
}

# 	Don't go to Phase II if mean(X[Y==1]) <= mean(X[Y==0])
	if(test < 3 & d.update(d0) < 1) {blrb7(); endi=1;}

	# Read here n2: the number of Phase II (D-Optimal) tests to run
	
	if(endi == 0 & n2n3 != 2 & n2n3 != 3 & n2n3 != 4 & n2n3 != 5)
	{
		gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		xx=paste("\nPhase I complete, (Mu, Sig) = (",g1,", ",g2,").\nEnter Phase II (D-Optimal) size n2: ",sep="");
		if(n2 == 0) {n2=as.numeric(readline(xx)); if(n2==0)n2n3=2; cat("\n");}			
		if(n2 < 0) {n2=0; endi=1;}
		if(n2 > 0) 
		{ 
		en[2]=n2;

		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		w=phaseII(d0,dat0,n2,reso,about,titl,unit,ln,term1); d0=w[[1]]; dat0=w[[2]]; endi=w[[3]];
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		}
	}	
	
	if(endi == 0)
		{ 
		if(n3 == 0)
			{
			gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
			jkl="complete"; if(n2 == 0) jkl="skipped";
			zz="";
			if(ln)zz=paste("\n\n** Starting values (tau2[1] & be) for Phase III, ln=T may need tweaking.\n",sep="");
			xx=paste("\nPhase II ",jkl,", (Mu, Sig) = (",g1,", ",g2,").",zz,"\nEnter Phase III (S-RMJ) size n3: ",sep="");
			xx=readline(xx); n3=as.numeric(xx);	
			if(n3 == 0)endi=8;
			if(n3 < 0){n3=0; endi=1;}
			about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
			}
		}

	if(endi == 0 & n3 > 0 & p == 0)
		{
		xx="Enter p lam: "; 
		if(p==0) 
			{
			xx=readline(xx);
			xx=strsplit(xx," ",fixed=T); 
			xx=as.numeric(xx[[1]]);
			nxx=length(xx);
			p=xx[1]; if(nxx > 1)lam=xx[2];
			cat("\n");
			}
		if(p >= 1 | p <= 0 | lam <= 0 | nxx != 2)
			{p=0; lam=0; endi=1; n2n3=3; if(n2 > 0) n2n3=6; }
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		}
	if(endi == 0) {
		n2n3=4; if(n2 > 0) n2n3=7;
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		w=sphaseIII(d0,dat0,n3,p,reso,about,titl,unit,ln,lam); d0=w[[1]]; 
		dat0=w[[2]]; endi=w[[3]]; jvec=w[[4]];	}

		#	Adapted from bottom of prd0	
		if(endi == 0 | endi == 2 | endi == 8) {
		gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
		xx=paste("\nPhase III complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
		cat(xx);
		} else cat("Test Suspended\n")

	en=c(n1,n2,n3);
	gg=glmmle(d0); g0=gg$one23; g1=round(gg$mu,7); g2=round(gg$sig,7);
	if(g0 == 1) musig=c(g1,g2) else musig=NULL;

	if(test == 1 | test == 2)
	{
	ret=list(d0,about,titl,ttl0,ttl1,ttl2,unit,en,p,reso,n2n3,ln,init,lam,test,savinit,jvec,term1,en12,musig);
	names(ret)=c("d0","about","title","ttl0","ttl1","ttl2","units","en","p","reso","n2n3","ln","init","lam","test","savinit","jvec","term1","en12","musig");
	} 
	if(test == 3 | test == 4)
	{
	ret=list(d0,about,titl,ttl0,ttl1,ttl2,unit,en,p,reso,n2n3,ln,init,lam,test,savinit,jvec,term1,en12,BL,dud,lev,musig);
	names(ret)=c("d0","about","title","ttl0","ttl1","ttl2","units","en","p","reso","n2n3","ln","init","lam","test","savinit","jvec","term1","en12","BL","dud","lev","musig");
	}
	if(endi == 2 & n1 > 1) ptest(ret,1); 

#------------------------------------------------------------------------------------
}

if(lY > 0)
{
#------------------------------------------------------------------------------------

	if(test == 1)
{
	if(endi == 0)
	{w=phaseBI1(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,Y,X); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]]; Y=w[[5]]; X=w[[6]];}	

	if(endi == 0)
	{w=phaseBI2(d0,dat0,sg,reso,about,titl,unit,ln,term1,Y,X); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]]; Y=w[[5]]; X=w[[6]];}
	en12[1]=nrow(d0);
	
	if(endi == 0)
	{w=phaseBI3(d0,dat0,sg,reso,about,titl,unit,ln,term1,Y,X);
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; Y=w[[4]]; X=w[[5]];}
	n1=nrow(d0); en12[2]=n1-en12[1];
} 
	if(test == 2)
{
	if(endi == 0)
	{w=nphaseBI(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,term1,Y,X); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; sg=w[[4]]; Y=w[[5]]; X=w[[6]];}	
	n1=en12[1]=nrow(d0);
}
	if(test == 3)
{
	if(endi == 0)
	{w=bphaseBI(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,BL,Y,X); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; Y=w[[4]]; X=w[[5]]; en12=w[[6]];}	
	n1=nrow(d0);
}

	if(test == 4)
{
	if(endi == 0)
	{w=lphaseBI(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,BL,Y,X); 
	d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; Y=w[[4]]; X=w[[5]]; en12=w[[6]];}	
	n1=nrow(d0);
}

# 	Don't go to Phase II if mean(X[Y==1]) <= mean(X[Y==0])
	if(test < 3 & d.update(d0) < 1) {blrb7(); endi=1;}

	# Read here n2: the number of Phase II (D-Optimal) tests to run
	
	if(endi == 0 & n2n3 != 2 & n2n3 != 3 & n2n3 != 4 & n2n3 != 5)
	{
		gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		xx=paste("\nPhase I complete, (Mu, Sig) = (",g1,", ",g2,").\nEnter Phase II (D-Optimal) size n2: ",sep="");
		if(n2 == 0) {n2=as.numeric(readline(xx)); if(n2==0)n2n3=2; cat("\n");}			
		if(n2 < 0) {n2=0; endi=1;}
		if(n2 > 0) 
		{ 
		en[2]=n2;

		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		w=phaseBII(d0,dat0,n2,reso,about,titl,unit,ln,term1,Y,X); d0=w[[1]]; dat0=w[[2]]; endi=w[[3]]; Y=w[[4]]; X=w[[5]];
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		}
	}	
	
	if(endi == 0)
		{ 
		if(n3 == 0)
			{
			gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
			jkl="complete"; if(n2 == 0) jkl="skipped";
			zz="";
			if(ln)zz=paste("\n\n** Starting values (tau2[1] & be) for Phase III, ln=T may need tweaking.\n",sep="");
			xx=paste("\nPhase II ",jkl,", (Mu, Sig) = (",g1,", ",g2,").",zz,"\nEnter Phase III (S-RMJ) size n3: ",sep="");
			xx=readline(xx); n3=as.numeric(xx);	
			if(n3 == 0)endi=8;
			if(n3 < 0){n3=0; endi=1;}
			about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
			}
		}

	if(endi == 0 & n3 > 0 & p == 0)
		{
		xx="Enter p lam: "; 
		if(p==0) 
			{
			xx=readline(xx);
			xx=strsplit(xx," ",fixed=T); 
			xx=as.numeric(xx[[1]]);
			nxx=length(xx);
			p=xx[1]; if(nxx > 1)lam=xx[2];
			cat("\n");
			}
		if(p >= 1 | p <= 0 | lam <= 0 | nxx != 2)
			{p=0; lam=0; endi=1; n2n3=3; if(n2 > 0) n2n3=6; }
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		}
	if(endi == 0) {
		n2n3=4; if(n2 > 0) n2n3=7;
		about=wabout(c(savinit,n1,n2,n3,p,lam,reso));
		w=sphaseBIII(d0,dat0,n3,p,reso,about,titl,unit,ln,Y,X,lam); d0=w[[1]]; 
		dat0=w[[2]]; endi=w[[3]]; jvec=w[[4]]; Y=w[[5]]; X=w[[6]];	}

		#	Adapted from bottom of prd0	
		if(endi == 0 | endi == 2 | endi == 8) {
		gg=glmmle(d0); g1=round(gg$mu,5); g2=round(gg$sig,5);
		xx=paste("\nPhase III complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
		cat(xx);
		} else cat("Test Suspended\n")

	en=c(n1,n2,n3);
	jt=F; about1=chabout(about,en,4:6); if(about != about1) {jt=T; about=about1;}
	gg=glmmle(d0); g0=gg$one23; g1=round(gg$mu,7); g2=round(gg$sig,7);
	if(g0 == 1) musig=c(g1,g2) else musig=NULL;

	if(test == 1 | test == 2)
	{
	ret=list(d0,about,titl,ttl0,ttl1,ttl2,unit,en,p,reso,n2n3,ln,init,lam,test,savinit,jvec,term1,en12,musig);
	names(ret)=c("d0","about","title","ttl0","ttl1","ttl2","units","en","p","reso","n2n3","ln","init","lam","test","savinit","jvec","term1","en12","musig");
	} 
	if(test == 3 | test == 4)
	{
	ret=list(d0,about,titl,ttl0,ttl1,ttl2,unit,en,p,reso,n2n3,ln,init,lam,test,savinit,jvec,term1,en12,BL,dud,lev,musig);
	names(ret)=c("d0","about","title","ttl0","ttl1","ttl2","units","en","p","reso","n2n3","ln","init",
	"lam","test","savinit","jvec","term1","en12","BL","dud","lev","musig");
	}
	if((jt | endi == 2) & n1 > 1) ptest(ret,1);

#------------------------------------------------------------------------------------
}
		
	rd0=d0; rd0$X=round(rd0$X,5); names(rd0)[1]="i,X"; rd0$EX=round(rd0$EX,5);
	write.table(rd0,file="gonogo.txt",quote=F,sep=",",na="i");
	return(ret);
}

phaseI1=function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln)
{
nret=c("d0","dat0","endi","sg");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;
endi=0;
mi=c(mlo,mhi);
a=matrix(c(.75,.25,.25,.75),ncol=2,byrow=T);
xx=t(a%*%mi);
	
for(i in 1:2)	{u=getd0(xx[i],d0,dat0,"I1",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi; if(endi == 1) break; }
if(endi == 0)
{
x=d0$X; y=d0$Y;

i1=0;

if(all(y==c(0,0)))
	{
		while(1)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=mi[2]+1.5*i1*sg; 
		u=getd0(xx,d0,dat0,"I1(i)",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;	
		if(d0$Y[nrow(d0)] == 1 | endi == 1) break;
		}
	}		

if(all(y==c(1,1)))
	{
		while(1)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=mi[1]-1.5*i1*sg; 
		u=getd0(xx,d0,dat0,"I1(ii)",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;		
		if(d0$Y[nrow(d0)] == 0 | endi == 1) break;
		}
	}

if(all(y==c(0,1))) d0$ID=rep("I1(iii)",length(y));

if(all(y==c(1,0)))
	{
		xx=c(mlo-3*sg,mhi+3*sg);
		for(i in 1:2)
			{
			u=getd0(xx[i],d0,dat0,"I1(iv)",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;
			if(endi == 1) break;
			}
	}	
}
ret=list(d0,dat0,endi,sg);
names(ret)=nret; 
return(ret);
}

phaseI2=function(d0,dat0,sg,reso,about,titl,unit,ln,term1)
{
nret=c("d0","dat0","endi","sg");
xw = paste("    ** 3pod would normally enter I3 here **\n", sep = "");
sav=0;
endi=0;
idii="";
while(1)
	{
	# del = m1-M0; del < 0 ==> OVERLAP
	j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
	while(del >= 1.5*sg & endi == 0)
		{
		if(endi == 0)
			{
			xx=(M0+m1)/2; 
			id=paste(idii,"I2(ib)",sep="");
			u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;		
			j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
			}
		}
	if(del < 0 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret)}
	j=n.update(d0); n0=j$n0; n1=j$n1;
	if(del >= 0 & endi == 0) 
		{
		ixw=0;
		if(n0 > n1 & endi == 0)
			{
			xx=m1+0.3*sg;
			id=paste(idii,"I2(ic)",sep="");
			u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;
			if(d0$Y[nrow(d0)] == 0 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; ixw=1;}
			if(ixw == 1){
			if(term1) return(ret) else			
			{if(ixw == 1 & sav == 0) {cat(xw); sav=1; ixw=0}; if(ok1(d0,0) | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret); } } 
			}
			if(d0$Y[nrow(d0)] == 1 & endi == 0)
				{
				xx=M0-.3*sg;	
				id=paste(idii,"I2(ic)",sep="");
				u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;
				if(d0$Y[nrow(d0)] == 1 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; ixw=1;}
				if(ixw == 1){
				if(term1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret)} else
				{if(ixw == 1 & sav == 0) {cat(xw); sav=1; ixw=0}; if(ok1(d0,1) | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret);  } }
				}
				if(d0$Y[nrow(d0)] == 0 & endi == 0)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		if(n0 <= n1 & endi == 0)
			{
			xx=M0-.3*sg;
			id=paste(idii,"I2(id)",sep="");
			u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;
			if(d0$Y[nrow(d0)] == 1 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; ixw=1;}
			if(ixw == 1){
			if(term1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret);} else
			{if(ixw == 1 & sav == 0) {cat(xw); sav=1; ixw=0}; if(ok1(d0,1) | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret); } }
			}
			if(d0$Y[nrow(d0)] == 0 & endi == 0)
				{
				xx=m1+.3*sg;	
				id=paste(idii,"I2(id)",sep="");
				u=getd0(xx,d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;	
				if(d0$Y[nrow(d0)] == 0 | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; ixw=1;}
				if(ixw == 1){
				if(term1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret);} else 
				{if(ixw == 1 & sav == 0) {cat(xw); sav=1; ixw=0}; if(ok1(d0,0) | endi == 1) {ret=list(d0,dat0,endi,sg); names=nret; return(ret); } }
				}
				if(d0$Y[nrow(d0)] == 1 & endi == 0)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		}
	}
}

phaseI3=function(d0,dat0,sg,reso,about,titl,unit,ln,term1)
{
j=m.update(d0); M0=j$M0; m1=j$m1; del=m1-M0;
xx=(M0+m1)/2;
if(sg+del > 0)xx=xx+c(1,-1)*sg/2;
lxx=length(xx)
for(i in 1:lxx)
	{
	if(i < lxx) u=getd0(xx[i],d0,dat0,"I3",reso,about,titl,unit,ln); 
	if(i == lxx) u=getd0(xx[i],d0,dat0,"I3",reso,about,titl,unit,ln,cab=T);	
	d0=u$d0; dat0=u$dat0; endi=u$endi;
if(!term1 & !ok1(d0)) endi=1;
	if(endi == 1) break;		
	}
ret=list(d0,dat0,endi);
names(ret)=c("d0","dat0","endi");		
return(ret);
}

nphaseI=function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,term1)
{
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

del=(mhi-mlo)/6;

eps=1e-007
n=0;
endi=0;
bl=c("B0","B1","B2","B3","B4");

# lf is a flag to adjust X1 & X2 in the ln=T neyer case. Use of it makes it deviate a tad from
# a true neyer conducted on the logs - but this way X1 & X2 stay the same in both ln settings
lf=0; 

while(endi == 0)
	{
	# PART 1 ************************************************************
	if(n == 0) block=0 else
		{
		j=n.update(d0); k0=j$n0; k1=j$n1;
		xlo=min(d0$X)
		xhi=max(d0$X)
		if(k1 <= eps) block=1 else
			{
			if(k1 >= n-eps) block=2 else
				{
				# PART 2 **************************************************
				j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
				dif = round(m1-M0,14)
				if(dif > sg) block=3 else 
					{
					if(dif >= 0) block=4 else block=5;
					}
				}
			}
		}
	# First
	if(block == 0) if(!ln)xbef = (mlo+mhi)/2 else {v=ifg(mlo,mhi); xbef=log((v[1]+v[2])/2); lf=1;}
	# All 0's
	if(block == 1) if(lf == 0) xbef = max(c((mhi + xhi)/2, xhi + 2 * sg, 2 * xhi - xlo)) else
	{xbef=log((v[1]+3*v[2])/4); lf=0;}
	# All 1's
	if(block == 2) if(lf == 0) xbef = min(c((mlo + xlo)/2, xlo - 2 * sg, 2 * xlo - xhi)) else
	{xbef=log((3*v[1]+v[2])/4); lf=0;}
	if(block == 3) xbef = (m1 + M0)/2
	if(block == 4) 
	{ 
	m=(m1+M0)/2; es=sg; sg=.8*sg;
	m = max(xlo, min(m, xhi))
	es = min(es,(xhi-xlo))
	b = yinfomat(d0,m,es)$infm
	xbef = m + kstar(b)*es
	}
	if(block == 5) {about=chabout(about,nrow(d0),5); if(term1) break else {if(ok1(d0)) break};}
	u=getd0(xbef,d0,dat0,bl[block+1],reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;
	n=nrow(d0);		
}
ret=list(d0,dat0,endi,sg);
nret=c("d0","dat0","endi","sg");
names(ret)=nret; 
return(ret);
}

bphaseI=function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,BL)
{
nRev=BL[1];  I=BL[2:3]; I[I == 0] =-1;
X=c(1,0);
nSeq=1+(I-I%%2)/2;
wz=which(I == -1);
if(length(wz) == 1) {X=X[-wz]; I=I[-wz]; nSeq=nSeq[-wz];}
lox=length(X);
udid=numeric(0);

nret=c("d0","dat0","endi","en12");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

en12=0;

for(ijk in 1:lox)
{
if(X[ijk] == 0) GE5=F else GE5=T;

# L=intToBitVect(nL), L is a List, nl is a vector of integers
# D's are the first 1:nSeq, U's are the last 1 or 2 depending on nAdd being 0 or 1, resp.

L=udli(I[ijk]);
nL=bintodec(L);

pl=zpfun(I[ijk]);
if(!GE5) pl=1-pl;
xx=mlo;

# initialize counter and accumulator
icnt=iacc=0;
dn=0; endi=0; ud=xud=numeric(0); yseq=numeric(0);

while(dn == 0 & endi == 0)
{
u=getd0(xx,d0,dat0,"IB",reso,about,titl,unit,ln); 

d0=u$d0; dat0=u$dat0; endi=u$endi; 
icnt=icnt+1; nx=length(d0$X); iacc=iacc+d0$X[[nx]];
if(endi == 1) break;
y=d0$Y;
ny=length(y);

if(GE5) {yseq=c(yseq,y[ny]); udid=c(udid,y[ny]);} else 
{yseq=c(yseq,1-y[ny]); udid=c(udid,1-y[ny]);}

lseq=list(yseq); ns=bintodec(lseq);

iw=which(nL == ns);

# Also will want to know if overlap has been achieved
j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
dif = round(m1-M0,14);
if(!is.na(dif) & dif < 0 & nRev < 0) dn=1;

if(length(iw) == 1 & dn == 0)
  {
xx=iacc/icnt;
xud=c(xud,xx);
nxud=length(xud);
kay=floor((xx-mlo)/sg);
pstar=(xx-mlo)/sg-floor((xx-mlo)/sg);
g=mlo+kay*sg;

if(GE5)
{
if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=-2; if(pstar <= .5) xx=g-sg else xx=g;}
if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=2; if(pstar <= .5) xx=g+sg else xx=g+2*sg;}
} else
{
if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=2; if(pstar <= .5) xx=g+sg else xx=g+2*sg;}
if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=-2; if(pstar <= .5) xx=g-sg else xx=g;}
}
icnt=iacc=0;

  yseq=numeric(0);
  cud=sum(abs(diff(ud)));
  if(!is.na(dif) & nRev > 0) if(cud >= nRev & dif < 0) dn=1;
  if(!is.na(dif) & nRev == 0) if(dif < 0) dn=1;

# At this point, if dn = 1, then the reversal and overlap criteria are met
# Reset dn = 0 if Avg(X[Y==1]) <= Avg(X[Y==0]), i.e., if d.update(d0) < 1
  if(d.update(d0) < 1) dn=0;
  }
}
en12=c(en12,nx);
}
en12=diff(en12);
udid[abs(udid) != 2]=0;
udid[udid==-2]="D"; udid[udid==2]="U"; udid[udid == 0]="";
d0$ID=udid;
ret=list(d0,dat0,endi,en12);
names(ret)=nret;
return(ret);
}

lphaseI=function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,BL)
{
nRev=BL[1];  I=BL[2:3]; I[I == 0] =-1;
X=c(1,0);
nSeq=1+(I-I%%2)/2;
wz=which(I == -1);
if(length(wz) == 1) {X=X[-wz]; I=I[-wz]; nSeq=nSeq[-wz];}
lox=length(X);
udid=numeric(0);

nret=c("d0","dat0","endi","en12");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

en12=0; 

for(ijk in 1:lox)
{
if(X[ijk] == 0)GE5=F else GE5=T;

# L=intToBitVect(nL), L is a List, nl is a vector of integers
# D's are the first 1:nSeq, U's are the last 1 or 2 depending on nAdd being 0 or 1, resp.

L=udli(I[ijk]);
nL=bintodec(L);

pl=zpfun(I[ijk]);
if(!GE5) pl=1-pl;
xx=mlo*(1-pl)+mhi*pl;

# initialize counter and accumulator
icnt=iacc=0;
dn=0; endi=0; ud=xud=numeric(0); yseq=numeric(0);

while(dn == 0 & endi == 0)
{
u=getd0(xx,d0,dat0,"IL",reso,about,titl,unit,ln);
d0=u$d0; dat0=u$dat0; endi=u$endi;
icnt=icnt+1; nx=length(d0$X); iacc=iacc+d0$X[[nx]];
if(endi == 1) break;

y=d0$Y;
ny=length(y);

if(GE5) {yseq=c(yseq,y[ny]); udid=c(udid,y[ny]);} else 
{yseq=c(yseq,1-y[ny]); udid=c(udid,1-y[ny]);}

lseq=list(yseq); ns=bintodec(lseq);

iw=which(nL == ns);

# Also will want to know if overlap has been achieved
j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
dif = round(m1-M0,14);
if(!is.na(dif) & dif < 0 & nRev < 0) dn=1;

if(length(iw) == 1 & dn == 0)
  {
    if(GE5)
	{
	if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=-2;}
	if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=2;}
	} else
	{
	if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=2;}
	if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=-2;}
	}

  xud=c(xud,iacc/icnt);
  nxud=length(xud);
  rud=2*rev(ud)-1;
  uc=cumsum(rud);
  ic=which(uc == 0);
# Calculate xxa, xx, reset counters
  if(length(ic) == 0)
  {
  if(GE5)
  {
  if(iw > nSeq[ijk]) xxa=mlo;
  if(iw <= nSeq[ijk]) xxa=mhi;
  } else
  {
  if(iw > nSeq[ijk]) xxa=mhi;
  if(iw <= nSeq[ijk]) xxa=mlo;
  }
  }
  if(length(ic) > 0) {ic=ic[1]; xxa=rev(xud); xxa=xxa[ic];}
  xx=(xud[nxud]+xxa)/2;

  icnt=iacc=0;
  yseq=numeric(0);
  cud=sum(abs(diff(ud)));
  if(!is.na(dif) & nRev > 0) if(cud >= nRev & dif < 0) dn=1;
  if(!is.na(dif) & nRev == 0) if(dif < 0) dn=1;
# At this point, if dn = 1, then the reversal and overlap criteria are met
# Reset dn = 0 if Avg(X[Y==1]) <= Avg(X[Y==0]), i.e., if d.update(d0) < 1
  if(d.update(d0) < 1) dn=0;
  }
}
en12=c(en12,nx);
}
en12=diff(en12);
udid[abs(udid) != 2]=0;
udid[udid==-2]="D"; udid[udid==2]="U"; udid[udid == 0]="";
d0$ID=udid

ret=list(d0,dat0,endi,en12);
names(ret)=nret;
return(ret);
}

phaseII=function(d0,dat0,n2,reso,about,titl,unit,ln,term1)
{
xl=xu=xstar=mu2=mu4=sg2=sg4=rep(0,n2);
for(i in 1:n2)
	{
	nq=glmmle(d0);
	mu2[i]=nq$mu;
	sg2[i]=nq$sig;
	xl[i]=min(d0$X); xu[i]=max(d0$X);
	mu4[i]=max(xl[i],min(mu2[i],xu[i]));
	sg4[i]=min(sg2[i],xu[i]-xl[i]);
	b=yinfomat(d0,mu4[i],sg4[i])$infm;
	xstar[i]=mu4[i]+kstar(b)*sg4[i];
	id="II1";
	if(i > 1) id="II2";
	u=getd0(xstar[i],d0,dat0,id,reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;
	if(term1) {if(endi == 1) break;} else 
	if(ok1(d0)$tf | endi == 1) break;
	}
ret=list(d0,dat0,endi);
names=c("d0","dat0","endi");		
return(ret);
}

sphaseIII=function(d0,dat0,n3,p,reso,about,titl,unit,ln,lam=0)
{
endi=0;
nret=c("d0","dat0","endi","jvec"); 
jvec=matrix(rep(0,10*(n3+1)),ncol=10);
nq=glmmle(d0);
mu=nq$mu; sig=nq$sig;

# Calculate initial tau1^2
# this variance/covariance matrix (vcov1) is scale free
ww=yinfomat(d0,mu,sig,solv=T);
tau2=sum(t(c(1,qnorm(p)^2))*diag(ww$vcov1));

# Truncate tau2[1]
ti=round((c(3,5)/qnorm(.975))^2,4)*sig^2;

#** NEW 
if(ln) ti=round((c(3,5)/qlnorm(.975))^2,4)*sig^2;

tau2=min(max(tau2,ti[1]),ti[2]);
	
# Use Mu Tilda and Sigma Tilda instead of Mu Hat and Sigma Hat 
m1=min(d0$X,na.rm=T); 
m2=max(d0$X,na.rm=T);
m2=min(c(mu,m2),na.rm=T);
mut=max(c(m1,m2),na.rm=T);
sigt=min(sig,diff(range(d0$X)),na.rm=T);
	
# Beta = 1/(2* SigmaTilda) per pp 9. You get beta = 0.4302985
be=1/(2*sigt);

#** NEW 
if(ln) be=(plnorm(qlnorm(p)))/(pnorm(qnorm(p))*sigt)

c1=f3point8(lam);
nu=sqrt(tau2)*c1;
	
xx=mut+qnorm(p)*sigt+nu;
u=getd0(xx,d0,dat0,"III1",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; 
ny=length(d0$Y); yy=d0$Y[ny];
jvec[1,]=c(0,0,0,0,0,tau2,nu,0,xx,yy);

endi=u$endi;

if(endi != 1)
{
	for(i in 1:n3)
	{
              # Compute next X|d0
	vv=skewL(c1,nu,tau2,p,be);
	a=vv[5]; tau2=vv[6]; nu=vv[7]; b=vv[8];
	xx=d0$X[nrow(d0)]-a*(d0$Y[nrow(d0)]-b);

	if(i < n3)
		{
		u=getd0(xx,d0,dat0,"III2",reso,about,titl,unit,ln); d0=u$d0; dat0=u$dat0; endi=u$endi;
		ny=length(d0$Y);
		yy=d0$Y[ny]
		jvec[i+1,]=c(vv,xx,yy);
		if(endi == 1) {ret=list(d0,dat0,endi,jvec); names(ret)=nret; return(ret);}
		}
	if(i == n3)
		{
		d0=rbind(d0,d0[nrow(d0),]);
		d0[nrow(d0),1:6]=c(0,0,0,round(xx,5),0,0);
		d0$ID[nrow(d0)]="III3";
		jvec[i+1,]=c(vv,xx,NA);
		endi=2;
		}
	}
}
jvec=data.frame(jvec);
names(jvec)=c("j","k","v","u","a","tau2","nu","b","x","y");
ret=list(d0,dat0,endi,jvec); 
names(ret)= nret;
return(ret);
}

# added cab to define n1 in the title of the graph produced by ptest 

getd0=function(xx,d0,dat0,ID,reso,about,titl,unit,ln,cab=F)
{
nret=c("d0","dat0","endi");
mret=c("d0","about","title","units","ln");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6))); d1=cbind(d1,"END"); 
names(d1)=names(d0)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d1[-1,];
n0=nrow(dat0); nd0=nrow(d0)+1;
endi=0;
if(n0 == 0)
	{
	u=getxr(xx,nd0,reso,ln);
	d1[1,1:6]=c(u[1:2],1,u[3],xx,u[4]); d1$ID=ID;
	if(u[2]*(1-u[2])!=0)
		{endi=1; ret=list(d0,dat0,endi); names(ret)=nret; 
		ret5=list(d0,about,titl,unit,ln); names(ret5)=mret; 
		if(nrow(d0) > 0) {ptest(ret5,1);} 
		return(ret);
		}	
	} 	
if(n0 > 0)	{d1=dat0[1,]; dat0=dat0[-1,]; if(is.null(dat0)) dat0=d1[-1,]; n0=nrow(dat0);}
d0=rbind(d0,d1);
ret=list(d0,dat0,endi);
if(cab)about=chabout(about,nrow(d0),4);
ret5=list(d0,about,titl,unit,ln); names(ret5)=mret;
if(nrow(d0) > 1) ptest(ret5,1);	
names(ret)=nret;
return(ret);
}

getxr=function(x,nd0,reso,ln)
{
buf=" ";
if(ln) x=exp(x);
if(nd0>9)buf="";
rx=round(x,5); if(reso > 0)rx=round(x/reso)*reso;
xx = paste(buf,nd0,". Test at X ~ ",rx,". Enter X & R: ", sep = "");
xx=readline(xx);
xx=as.numeric(unlist(strsplit(xx," ")));
tx=xx[1];
if(ln) xx[1]=round(log(tx),5);
return(c(xx,rx,tx));
}

phaseBI1=function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,Y,X)
{
lY=length(Y); lX=length(X);
nret=c("d0","dat0","endi","sg","Y","X");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;
endi=0;
mi=c(mlo,mhi);
a=matrix(c(.75,.25,.25,.75),ncol=2,byrow=T);
xx=t(a%*%mi);
	
ym=min(lY,2); xm=min(lX,2);
for(i in 1:ym)
{
if(lX < i) X=NULL
u=getBd0(xx[i],d0,dat0,"I1",reso,about,titl,unit,ln,Y[i],X[i]);
d0=u$d0; dat0=u$dat0; endi=u$endi;
}
Y=Y[-c(1:ym)]; if(length(X) > xm) X=X[-c(1:xm)] else X=NULL; lY=length(Y); if(lY == 0) endi=1;

if(endi == 0)
{
x=d0$X; y=d0$Y;
i1=0;
if(all(y==c(0,0)))
	{
		while(lY > 0)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=mi[2]+1.5*i1*sg; 
		u=getBd0(xx,d0,dat0,"I1(i)",reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;
		Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;	
		if(d0$Y[nrow(d0)] == 1 | endi == 1) break;
		}
	}		

if(all(y==c(1,1)))
	{
		while(lY > 0)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=mi[1]-1.5*i1*sg; 
		u=getBd0(xx,d0,dat0,"I1(ii)",reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;		
		Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
		if(d0$Y[nrow(d0)] == 0 | endi == 1) break;
		}
	}

if(all(y==c(0,1))) d0$ID=rep("I1(iii)",length(y));

if(all(y==c(1,0)))
	{
		xx=c(mlo-3*sg,mhi+3*sg);
		for(i in 1:min(lY,2))
			{
			u=getBd0(xx[i],d0,dat0,"I1(iv)",reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;
			Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
			if(endi == 1) break;
			}
	}	
}
ret=list(d0,dat0,endi,sg,Y,X);
names(ret)=nret; 
return(ret);
}

phaseBI2=function(d0,dat0,sg,reso,about,titl,unit,ln,term1,Y,X)
{
nret=c("d0","dat0","endi","sg","Y","X");
xw = paste("    ** 3pod would normally enter I3 here **\n", sep = "");
sav=0;
endi=0;
idii="";
while(1)
	{
	# del = m1-M0; del < 0 ==> OVERLAP
	j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
	while(del >= 1.5*sg & endi == 0)
		{
		if(endi == 0)
			{
			xx=(M0+m1)/2; 
			id=paste(idii,"I2(ib)",sep="");
			u=getBd0(xx,d0,dat0,id,reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;		
			Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
			j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
			}
		}
	if(del < 0 | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; return(ret)}
	j=n.update(d0); n0=j$n0; n1=j$n1;
	if(del >= 0 & endi == 0) 
		{
		ixw=0;
		if(n0 > n1 & endi == 0)
			{
			xx=m1+0.3*sg;
			id=paste(idii,"I2(ic)",sep="");
			u=getBd0(xx,d0,dat0,id,reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;
			Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
			if(d0$Y[nrow(d0)] == 0 | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; ixw=1;}
			if(ixw == 1){
			if(term1) return(ret) else			
			{if(ixw == 1 & sav == 0) {cat(xw); sav=1; ixw=0}; if(ok1(d0,0) | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; return(ret); } } 
			}
			if(d0$Y[nrow(d0)] == 1 & endi == 0)
				{
				xx=M0-.3*sg;	
				id=paste(idii,"I2(ic)",sep="");
				u=getBd0(xx,d0,dat0,id,reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;
				Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
				if(d0$Y[nrow(d0)] == 1 | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; ixw=1;}
				if(ixw == 1){
				if(term1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; return(ret)} else
				{if(ixw == 1 & sav == 0) {cat(xw); sav=1; ixw=0}; if(ok1(d0,1) | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; return(ret);  } }
				}
				if(d0$Y[nrow(d0)] == 0 & endi == 0)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		if(n0 <= n1 & endi == 0)
			{
			xx=M0-.3*sg;
			id=paste(idii,"I2(id)",sep="");
			u=getBd0(xx,d0,dat0,id,reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;
			Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
			if(d0$Y[nrow(d0)] == 1 | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; ixw=1;}
			if(ixw == 1){
			if(term1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; return(ret);} else
			{if(ixw == 1 & sav == 0) {cat(xw); sav=1; ixw=0}; if(ok1(d0,1) | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; return(ret); } }
			}
			if(d0$Y[nrow(d0)] == 0 & endi == 0)
				{
				xx=m1+.3*sg;	
				id=paste(idii,"I2(id)",sep="");
				u=getBd0(xx,d0,dat0,id,reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;	
				Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
				if(d0$Y[nrow(d0)] == 0 | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; ixw=1;}
				if(ixw == 1){
				if(term1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; return(ret);} else 
				{if(ixw == 1 & sav == 0) {cat(xw); sav=1; ixw=0}; if(ok1(d0,0) | endi == 1) {ret=list(d0,dat0,endi,sg,Y,X); names=nret; return(ret); } }
				}
				if(d0$Y[nrow(d0)] == 1 & endi == 0)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		}
	}
}

phaseBI3=function(d0,dat0,sg,reso,about,titl,unit,ln,term1,Y,X)
{
j=m.update(d0); M0=j$M0; m1=j$m1; del=m1-M0;
xx=(M0+m1)/2;
if(sg+del > 0)xx=xx+c(1,-1)*sg/2;
lxx=length(xx)
for(i in 1:lxx)
	{
	if(i < lxx) u=getBd0(xx[i],d0,dat0,"I3",reso,about,titl,unit,ln,Y[1],X[1]); 
	if(i == lxx) u=getBd0(xx[i],d0,dat0,"I3",reso,about,titl,unit,ln,Y[1],X[1],cab=T);	
	d0=u$d0; dat0=u$dat0; endi=u$endi;
	if(!term1 & !ok1(d0)) endi=1;
	Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
	if(endi == 1) break;		
	}
ret=list(d0,dat0,endi,Y,X);
names(ret)=c("d0","dat0","endi","Y","X");		
return(ret);
}

nphaseBI=function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,term1,Y,X)
{
lY=length(Y); lX=length(X);
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

del=(mhi-mlo)/6;

eps=1e-007
n=0;
endi=0;
bl=c("B0","B1","B2","B3","B4");

# lf is a flag to adjust X1 & X2 in the ln=T neyer case. Use of it makes it deviate a tad from
# a true neyer conducted on the logs - but this way X1 & X2 stay the same in both ln settings
lf=0; 

while(endi == 0)
	{
	# PART 1 ************************************************************
	if(n == 0) block=0 else
		{
		j=n.update(d0); k0=j$n0; k1=j$n1;
		xlo=min(d0$X)
		xhi=max(d0$X)
		if(k1 <= eps) block=1 else
			{
			if(k1 >= n-eps) block=2 else
				{
				# PART 2 **************************************************
				j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
				dif = round(m1-M0,14)
				if(dif > sg) block=3 else 
					{
					if(dif >= 0) block=4 else block=5;
					}
				}
			}
		}
	# First
	if(block == 0) if(!ln)xbef = (mlo+mhi)/2 else {v=ifg(mlo,mhi); xbef=log((v[1]+v[2])/2); lf=1;}
	# All 0's
	if(block == 1) if(lf == 0) xbef = max(c((mhi + xhi)/2, xhi + 2 * sg, 2 * xhi - xlo)) else
	{xbef=log((v[1]+3*v[2])/4); lf=0;}
	# All 1's
	if(block == 2) if(lf == 0) xbef = min(c((mlo + xlo)/2, xlo - 2 * sg, 2 * xlo - xhi)) else
	{xbef=log((3*v[1]+v[2])/4); lf=0;}
	if(block == 3) xbef = (m1 + M0)/2
	if(block == 4) 
	{ 
	m=(m1+M0)/2; es=sg; sg=.8*sg;
	m = max(xlo, min(m, xhi))
	es = min(es,(xhi-xlo))
	b = yinfomat(d0,m,es)$infm
	xbef = m + kstar(b)*es
	}
	if(block == 5) {about=chabout(about,nrow(d0),5); if(term1) break else {if(ok1(d0)) break};}
	u=getBd0(xbef,d0,dat0,bl[block+1],reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;
	Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
	n=nrow(d0);		
}
ret=list(d0,dat0,endi,sg,Y,X);
nret=c("d0","dat0","endi","sg","Y","X");
names(ret)=nret; 
return(ret);
}

bphaseBI=function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,BL,Y,Xx)
{
nRev=BL[1];  I=BL[2:3]; I[I == 0] =-1;
X=c(1,0);
nSeq=1+(I-I%%2)/2;
wz=which(I == -1);
if(length(wz) == 1) {X=X[-wz]; I=I[-wz]; nSeq=nSeq[-wz];}
lox=length(X);
udid=numeric(0);

nret=c("d0","dat0","endi","Y","Xx","en12");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

en12=0;

for(ijk in 1:lox)
{
if(X[ijk] == 0) GE5=F else GE5=T;

# L=intToBitVect(nL), L is a List, nl is a vector of integers
# D's are the first 1:nSeq, U's are the last 1 or 2 depending on nAdd being 0 or 1, resp.

L=udli(I[ijk]);
nL=bintodec(L);

pl=zpfun(I[ijk]);
if(!GE5) pl=1-pl;
xx=mlo;

# initialize counter and accumulator
icnt=iacc=0;
dn=0; endi=0; ud=xud=numeric(0); yseq=numeric(0);

while(dn == 0 & endi == 0)
{
u=getBd0(xx,d0,dat0,"",reso,about,titl,unit,ln,Y[1],Xx[1]); 

d0=u$d0; dat0=u$dat0; endi=u$endi; 
icnt=icnt+1; nx=length(d0$X); iacc=iacc+d0$X[[nx]];
Y=Y[-1]; if(length(Xx) > 1) Xx=Xx[-1] else Xx=NULL; lY=length(Y); if(lY == 0) endi=1;
y=d0$Y;
ny=length(y);

if(GE5) {yseq=c(yseq,y[ny]); udid=c(udid,y[ny]);} else 
{yseq=c(yseq,1-y[ny]); udid=c(udid,1-y[ny]);}

lseq=list(yseq); ns=bintodec(lseq);
iw=which(nL == ns);

# Also will want to know if overlap has been achieved
j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
dif = round(m1-M0,14);
if(!is.na(dif) & dif < 0 & nRev < 0) dn=1;

if(length(iw) == 1 & dn == 0)
  {
xx=iacc/icnt;
xud=c(xud,xx);
nxud=length(xud);
kay=floor((xx-mlo)/sg);
pstar=(xx-mlo)/sg-floor((xx-mlo)/sg);
g=mlo+kay*sg;

if(GE5)
{
if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=-2; if(pstar <= .5) xx=g-sg else xx=g;}
if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=2; if(pstar <= .5) xx=g+sg else xx=g+2*sg;}
} else
{
if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=2; if(pstar <= .5) xx=g+sg else xx=g+2*sg;}
if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=-2; if(pstar <= .5) xx=g-sg else xx=g;}
}
icnt=iacc=0;

  yseq=numeric(0);
  cud=sum(abs(diff(ud)));
  if(!is.na(dif) & nRev > 0) if(cud >= nRev & dif < 0) dn=1;
  if(!is.na(dif) & nRev == 0) if(dif < 0) dn=1;

# At this point, if dn = 1, then the reversal and overlap criteria are met
# Reset dn = 0 if Avg(X[Y==1]) <= Avg(X[Y==0]), i.e., if d.update(d0) < 1
  if(d.update(d0) < 1) dn=0;
  }
}
en12=c(en12,nx);
}
en12=diff(en12);
udid[abs(udid) != 2]=0;
udid[udid==-2]="D"; udid[udid==2]="U"; udid[udid == 0]="";
d0$ID=udid;
ret=list(d0,dat0,endi,Y,Xx,en12);
names(ret)=nret;
return(ret);			
}

lphaseBI=function(dat0,mlo,mhi,sg,reso,about,titl,unit,ln,BL,Y,Xx)
{
nRev=BL[1];  I=BL[2:3]; I[I == 0] =-1;
X=c(1,0);
nSeq=1+(I-I%%2)/2;
wz=which(I == -1);
if(length(wz) == 1) {X=X[-wz]; I=I[-wz]; nSeq=nSeq[-wz];}
lox=length(X);
udid=numeric(0);

nret=c("d0","dat0","endi","Y","Xx","en12");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

en12=0; 

for(ijk in 1:lox)
{
if(X[ijk] == 0)GE5=F else GE5=T;

# L=intToBitVect(nL), L is a List, nl is a vector of integers
# D's are the first 1:nSeq, U's are the last 1 or 2 depending on nAdd being 0 or 1, resp.

L=udli(I[ijk]);
nL=bintodec(L);

pl=zpfun(I[ijk]);
if(!GE5) pl=1-pl;
xx=mlo*(1-pl)+mhi*pl;

# initialize counter and accumulator
icnt=iacc=0;
dn=0; endi=0; ud=xud=numeric(0); yseq=numeric(0);

while(dn == 0 & endi == 0)
{
u=getBd0(xx,d0,dat0,"",reso,about,titl,unit,ln,Y[1],Xx[1]);
d0=u$d0; dat0=u$dat0; endi=u$endi; 
icnt=icnt+1; nx=length(d0$X); iacc=iacc+d0$X[[nx]];
Y=Y[-1]; if(length(Xx) > 1) Xx=Xx[-1] else Xx=NULL; lY=length(Y); if(lY == 0) endi=1;
y=d0$Y;
ny=length(y);

if(GE5) {yseq=c(yseq,y[ny]); udid=c(udid,y[ny]);} else 
{yseq=c(yseq,1-y[ny]); udid=c(udid,1-y[ny]);}

lseq=list(yseq); ns=bintodec(lseq);

iw=which(nL == ns);

# Also want to know if overlap has been achieved
j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
dif = round(m1-M0,14);
if(!is.na(dif) & dif < 0 & nRev < 0) dn=1;

if(length(iw) == 1 & dn == 0)
  {
  if(GE5)
	{
	if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=-2;}
	if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=2;}
	} else
	{
	if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=2;}
	if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=-2;}
	}

  xud=c(xud,iacc/icnt);
  nxud=length(xud);
  rud=2*rev(ud)-1;
  uc=cumsum(rud);
  ic=which(uc == 0);
  # Calculate xxa, xx, reset counters
  if(length(ic) == 0)
  {
  if(GE5)
  {
  if(iw > nSeq[ijk]) xxa=mlo;
  if(iw <= nSeq[ijk]) xxa=mhi;
  } else
  {
  if(iw > nSeq[ijk]) xxa=mhi;
  if(iw <= nSeq[ijk]) xxa=mlo;
  }
  }
  if(length(ic) > 0) {ic=ic[1]; xxa=rev(xud); xxa=xxa[ic];}
  xx=(xud[nxud]+xxa)/2;

  icnt=iacc=0;
  yseq=numeric(0);
  cud=sum(abs(diff(ud)));
  if(!is.na(dif) & nRev > 0) if(cud >= nRev & dif < 0) dn=1;
  if(!is.na(dif) & nRev == 0) if(dif < 0) dn=1;
# At this point, if dn = 1, then the reversal and overlap criteria are met
# Reset dn = 0 if Avg(X[Y==1]) <= Avg(X[Y==0]), i.e., if d.update(d0) < 1
  if(d.update(d0) < 1) dn=0;
  }
}
en12=c(en12,nx);
}
en12=diff(en12);
udid[abs(udid) != 2]=0;
udid[udid==-2]="D"; udid[udid==2]="U"; udid[udid == 0]="";
d0$ID=udid

ret=list(d0,dat0,endi,Y,Xx,en12); 	
names(ret)=nret;
return(ret);			
}

phaseBII=function(d0,dat0,n2,reso,about,titl,unit,ln,term1,Y,X)
{
xl=xu=xstar=mu2=mu4=sg2=sg4=rep(0,n2);
lY=length(Y);
for(i in 1:min(n2,lY))
	{
	nq=glmmle(d0);
	mu2[i]=nq$mu;
	sg2[i]=nq$sig;
	xl[i]=min(d0$X); xu[i]=max(d0$X);
	mu4[i]=max(xl[i],min(mu2[i],xu[i]));
	sg4[i]=min(sg2[i],xu[i]-xl[i]);
	b=yinfomat(d0,mu4[i],sg4[i])$infm;
	xstar[i]=mu4[i]+kstar(b)*sg4[i];
	id="II1";
	if(i > 1) id="II2";
	u=getBd0(xstar[i],d0,dat0,id,reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;
	Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
	if(term1) {if(endi == 1) break;} else 
	if(ok1(d0)$tf | endi == 1) break;
	}
ret=list(d0,dat0,endi,Y,X);
names=c("d0","dat0","endi","Y","X");		
return(ret);
}

sphaseBIII=function(d0,dat0,n3,p,reso,about,titl,unit,ln,Y,X,lam=0)
{
lY=length(Y);
endi=0;
nret=c("d0","dat0","endi","jvec","Y","X"); 
jvec=matrix(rep(0,10*(n3+1)),ncol=10);
nq=glmmle(d0);
mu=nq$mu; sig=nq$sig;

# Calculate initial tau1^2
# this variance/covariance matrix (vcov1) is scale free
ww=yinfomat(d0,mu,sig,solv=T);
tau2=sum(t(c(1,qnorm(p)^2))*diag(ww$vcov1));

# Truncate tau2[1]
ti=round((c(3,5)/qnorm(.975))^2,4)*sig^2;

#** NEW 
if(ln) ti=round((c(3,5)/qlnorm(.975))^2,4)*sig^2;

tau2=min(max(tau2,ti[1]),ti[2]);
        
# Use Mu Tilda and Sigma Tilda instead of Mu Hat and Sigma Hat 
m1=min(d0$X,na.rm=T); 
m2=max(d0$X,na.rm=T);
m2=min(c(mu,m2),na.rm=T);
mut=max(c(m1,m2),na.rm=T);
sigt=min(sig,diff(range(d0$X)),na.rm=T);
        
# Beta = 1/(2* SigmaTilda) per pp 9. You get beta = 0.4302985
be=1/(2*sigt);

#** NEW 
if(ln) be=plnorm(qlnorm(p))/(pnorm(qnorm(p))*sigt)

c1=f3point8(lam);
nu=sqrt(tau2)*c1;
        
xx=mut+qnorm(p)*sigt+nu;
u=getBd0(xx,d0,dat0,"III1",reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; 
ny=length(d0$Y); yy=d0$Y[ny];
jvec[1,]=c(0,0,0,0,0,tau2,nu,0,xx,yy);

endi=u$endi;
Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;

if(endi != 1)
{
mn3=min(n3,lY);
        for(i in 1:mn3)
        {
              # Compute next X|d0
        vv=skewL(c1,nu,tau2,p,be);
        a=vv[5]; tau2=vv[6]; nu=vv[7]; b=vv[8];
        xx=d0$X[nrow(d0)]-a*(d0$Y[nrow(d0)]-b);

        if(i < mn3)
                {
                u=getBd0(xx,d0,dat0,"III2",reso,about,titl,unit,ln,Y[1],X[1]); d0=u$d0; dat0=u$dat0; endi=u$endi;
                Y=Y[-1]; if(length(X) > 1) X=X[-1] else X=NULL; lY=length(Y); if(lY == 0) endi=1;
                ny=length(d0$Y);
                yy=d0$Y[ny]
                jvec[i+1,]=c(vv,xx,yy);
                if(endi == 1) {ret=list(d0,dat0,endi,jvec); names(ret)=nret; return(ret);}
                }
        if(i == mn3)
                {
                d0=rbind(d0,d0[nrow(d0),]);
                d0[nrow(d0),1:6]=c(0,0,0,round(xx,5),0,0);
                d0$ID[nrow(d0)]="III3";
                jvec[i+1,]=c(vv,xx,NA);
                endi=2;
                }
        }
}
jvec=data.frame(jvec);
names(jvec)=c("j","k","v","u","a","tau2","nu","b","x","y");
ret=list(d0,dat0,endi,jvec,Y,X); 
names(ret)= nret;
return(ret);
}

getBd0=function(xx,d0,dat0,ID,reso,about,titl,unit,ln,Y,X,cab=F)
{
nret=c("d0","dat0","endi");
mret=c("d0","about","title","units","ln");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6))); d1=cbind(d1,"END"); 
names(d1)=names(d0)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d1[-1,];
n0=nrow(dat0); nd0=nrow(d0)+1;
endi=0;
if(n0 == 0)
	{
	u=getBxr(xx,nd0,reso,ln,Y,X)
	d1[1,1:6]=c(u[1:2],1,u[3],xx,u[4]); d1$ID=ID;
	if(u[2]*(1-u[2])!=0)
		{endi=1; ret=list(d0,dat0,endi); names(ret)=nret; 
		ret5=list(d0,about,titl,unit,ln); names(ret5)=mret; 
		if(nrow(d0) > 0) {ptest(ret5,1);} 
		return(ret);
		}	
	} 	
if(n0 > 0)	{d1=dat0[1,]; dat0=dat0[-1,]; if(is.null(dat0)) dat0=d1[-1,]; n0=nrow(dat0);}
d0=rbind(d0,d1);
ret=list(d0,dat0,endi);
if(cab)about=chabout(about,nrow(d0),4);
ret5=list(d0,about,titl,unit,ln); names(ret5)=mret;
if(nrow(d0) > 1) ptest(ret5,1);	
names(ret)=nret;
return(ret);
}

getBxr=function(x,nd0,reso,ln,Y,X)
{
if(ln) x=exp(x); 
rx=round(x,5); if(reso > 0)rx=round(x/reso)*reso;
if(length(X) > 0) xx=c(X[1],Y) else xx=c(rx,Y)
tx=xx[1];
if(ln) xx[1]=round(log(tx),5);
return(c(xx,rx,tx));
}

prd0=function(z)
{
        test=z$test; 
	  d00=z$d0;
        en=z$en;
        pp=z$p;
	  llam=z$lam;
	  n2n3=z$n2n3;
        n=cumsum(en);
        n0=nrow(d00);
		ln=z$ln;
	  	u=d00$X;
		if(ln) u=round(exp(u),5);
        cat(paste("Enter title (without quotes): ",z$title,"\n",sep=""));
        cat(paste("Enter units (without quotes): ",z$units,"\n",sep=""));
        if(n0 == 0)return(n2n3)
	  if(test > 2)
	  {
	  a=paste(z$trans,collapse=" ");
	  xx=paste("Enter nRev nSeq nAdd X (without quotes): ",a,"\n",sep="");
	  cat(xx);
	  dud=prtrans(z$BL);
	  } else cat("\n")
        for(i in 1:n0)
        {
                buf=" ";if(i>9)buf="";
		    xx = paste(buf,i,". Test at X ~ ",d00$RX[i],". Enter X & R: ",u[i]," ",d00$Y[i],"\n", sep = "");
		    cat(xx);

                if(i == en[1] & (en[2] != 0 | n2n3 ==2)){
                gg=glmmle(d00[1:n[1],]); g1=round(gg$mu,5); g2=round(gg$sig,5);
                xx=paste("\nPhase I complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
                cat(xx);
                        
			if(en[2] == 0 & i == en[1] & n2n3 == 2)
                        {
 			xx=paste("Enter Phase II (D-Optimal) size n2: ",en[2],"\n",sep="");
                        cat(xx);
                        }

                        if(en[2] == 0 & en[3] > 0)
                        {
                        xx=paste("Enter Phase II (D-Optimal) size n2: ",en[2],"\n",sep="");
                        cat(xx);
                        }

                        if(en[2] > 0)
                        {
                        xx=paste("Enter Phase II (D-Optimal) size n2: ",en[2],"\n\n",sep="");
                        cat(xx);
                        }
                }
                        
                if(en[2] != 0 & i == n[2]){
                if(en[3]>0)
		    {
		    gg=glmmle(d00[1:n[2],]); g1=round(gg$mu,5); g2=round(gg$sig,5);
                    xx=paste("\nPhase II complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
                    cat(xx);
		    }
                        i7=0
                        if(en[3] > 0)
                        {
                        xx=paste("Enter Phase III (RMJ) size n3: ",en[3],"\n",sep="");
                        cat(xx);
                        i7=1
                        }
                        if(pp > 0 & pp < 1)
                        {
                        xx=paste("Enter Phase III (RMJ) size n3: ",en[3],"\n",sep="");
                        if(i7 == 0)cat(xx);
                        xx=paste("Enter p lam: ",pp," ",llam,"\n\n",sep=""); 
                        cat(xx);
                        }
                }

                if(en[2] == 0 & en[3] > 0 & i >= n[2] & n2n3 != 5){
 		    gg=glmmle(d00[1:n[2],]); g1=round(gg$mu,5); g2=round(gg$sig,5);
                xx=paste("\nPhase I complete, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
                cat(xx);
                xx=paste("Enter Phase II (D-Optimal) size n2: ",en[2],"\n",sep="");
                cat(xx);
		    xx=paste("\n\nPhase II skipped, (Mu, Sig) = (",g1,", ",g2,").\n",sep="");
                cat(xx);
                xx=paste("Enter Phase III (RMJ) size n3: ",en[3],"\n",sep="");
                cat(xx);

		    if(n2n3 == 4) {xx=paste("Enter p lam: ",pp," ",llam,"\n\n",sep=""); cat(xx); }
		    n2n3=5;	
                }
        }
return(n2n3)
}

d.update=function(dat)
{
x=dat[,1];
y=dat[,2];
x1=x[y==1];
n1=length(x1);
x0=x[y==0];
n0=length(x0);
if(n1*n0 != 0) return(sign(mean(x1)-mean(x0))) else return(0)
}

ok1=function(dat,y=2)
{
nr=nrow(dat);
yy=dat$Y[nr];
j1=m.update(dat);
j2=d.update(dat);
j3=sign(j1$M0-j1$m1);
if(j2 == 1 & j3 == 1) tf=T else tf=F;
if(y == 2) tf & yy==y;
return(tf)
}

chabout=function(about,s47,loc)
{
x=about;
x1=gsub("[|]",",",x);
x2=gsub("[{]","vv=c(",x1)
x3=gsub("[}]",")",x2)
eval(parse(text=paste(x3)))
vv[loc]=s47;
a=wabout(vv);
return(a)
}

wabout=function(vv)
{
f=",";
c=sub("^(-?)0.", "\\1.", vv);
about=paste("{",c[1],f,c[2],f,c[3],"|",c[4],f,c[5],f,c[6],"|",c[7],f,c[8],f,c[9],"}",sep="");
return(about);
}

blrb7=function()
{
x=
"
  Entry into Phase II requires that a positive and finite sigma exists.
  Thus, M0 > m1 (for overlap) & delta = Avg(X[Y==1]) - Avg(X[Y==0]) > 0. 
  The second condition ensures that the regression slope coefficient is 
  positive. Since your completed 3pod or Neyer Phase I did not meet both
  conditions, it has been suspended for your further review. Bruceton 
  and Langlie Phase I's have been programmed to continue on until both 
  conditions are met. 
    
"
cat(x)
return()
}

blrb8=function()
{
x=
"
  Going from I3 to Phase II requires that a positive, finite sigma exists. 
  Thus, M0 > m1 (for overlap) & delta = Avg(X[Y==1]) - Avg(X[Y==0]) > 0. 
  The second condition ensures that the regression slope coefficient is 
  positive. Since your completed 3pod or Neyer Phase I did not meet both
  conditions, it has been suspended for your further review. Bruceton 
  and Langlie Phase I's have been programmed to continue on until both 
  conditions are met. 
    
"
cat(x)
return()

}

about4=function(x)
{
x1=gsub("[|]",",",x);
x2=gsub("[{]","a=c(",x1);
x3=gsub("[}]",")",x2);
eval(parse(text=paste(x3)));
return(a[4:7]);
}

fixw=function(w,k=1)
{
# lop off the last k entries and resume test 
if(k < 1) return(w);
for(i in 1:k)w=fixw1(w)
return(w)
}

fixw1=function(w)
{
d0=w$d0;
n=nrow(d0);

about=w$about;
en=w$en;
p=w$p;
n2n3=w$n2n3;
test=w$test

l=about4(about);
l0=length(which(l==0));

if(l0 == 4)
{
	# Case 1
	cas=1; d0=d0[-n,]; if(n > 0)en[1]=n-1;
}

if(l0 == 3)
{
	# Case 2
	cas=2; d0=d0[-n,]; en[1]=en[1]-1; 
}

if(l0 == 2)
{
	if(n > l[1] & n < l[1]+l[2])
	{
	# Case 4;
	cas=4; d0=d0[-n,]; 
	}

	if(n == l[1]+l[2])
	{
	# Case 5;
	cas=5; d0=d0[-n,]; 
	}

	if(n == l[1])
	{
	# Case 3;
	cas=3; en[2]=0; 
	} 
}

if(l0 == 1)
{
	# Case 6
	cas=6; en[3]=0; n2n3=0; 
}

if(l0 == 0)
{
	if(n == l[1]+l[2])
	{
	# Case 7
	cas=7; p=0; n2n3=6; 
	}
	if(n > l[1]+l[2] & n <= l[1]+l[2]+l[3])
	{
	# Case 8
	cas=8; d0=d0[-n,]; 
	}
	if(n > l[1]+l[2]+l[3])
	{
	# Case 9
	cas=9; d0=d0[-n,]; d0=d0[-(n-1),]; 
	}
}

w$d0=d0;
w$en=en;
w$p=p;
w$n2n3=n2n3;

nen1=en[1]; if(en[2] == 0) nen1=0;

s47=c(nen1,en[2:3],p); loc=4:7;

if(cas > 1 | test == 2) w$about=chabout(about,s47,loc);

rd0=d0; rd0$X=round(rd0$X,5); names(rd0)[1]="i,X"; 
rd0$EX=round(rd0$EX,5);
rd0$TX=round(rd0$TX,5);
write.table(rd0,file="fixw.txt",quote=F,sep=",",na="i");

if(en[3] == 0)w$jvec=NULL;

return(w)
}

pdat1=function(dat,notitle=F,ud=F)
{
# when resuming, names(dat) = d0, about, title, units and ln. That's it!
# That's because ptest(dat,1) is called from getd0
dt=dtt=dat$d0; about=dat$about; titl=dat$title; unit=dat$unit; ln=dat$ln; 
ttl0=dat$ttl0; ttl1=dat$ttl1; ttl2=dat$ttl2; en12=dat$en12; en=dat$en;
h2=""; if(ln) h2="Log ";
# pee, neyer, & test aren't defined during the test. Need to infer them.
pee=dat$p; test=dat$test;
x=dt$X; y=dt$Y; id=dt$ID; nid=length(id);

if(is.null(about)) {cat("This function only works for lists created by gonogo\n\n"); return();}

newid="I"; test=34; oldids=id[nid];
if(is.element(id[1],c("I1","I1(i)","I1(ii)","I1(iii)","I1(iv)"))) test=1;

if(id[1] == "B0") test=2;

if(length(pee) == 0) pee=0;
fini=0; if(id[nid]=="III3") fini=1;
if(fini == 1) {dtt=dtt[-nid,]; x=x[-nid]; y=y[-nid]; id=id[-nid]; nid=nid-1;}
zee=x[1];
if(pee*(1-pee) > 0 & fini == 1) { yu=glmmle(dtt); zee=yu$mu+qnorm(pee)*yu$sig; }
about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[1],",",n[2],",",n[3],"|p,",lambda,",res}",sep=""));
w=pretty(x,n=10);	
ens=1:nid; rd=which(y==1); gr=which(y==0); 
ylm=range(pretty(c(x,max(x,na.rm=T)+diff(range(x))/80),n=10));

# for tick locations 
lb=nid-1; if(lb > 30) lb=ceiling(lb/2);

if(nid == 1) return();

ft=T;
if(nid > 1)
{
par(mar=c(4,4,5,2) + 0.1);
lnum=2.3;
if(!ln)plot(c(ens,1),c(x,zee),type="n",xlab="",ylab="",ylim=ylm,lab=c(lb,5,7)) else
{
par(mar=c(4,3,5,3) + 0.1);
plot(c(ens,1),c(x,zee),type="n",xlab="",ylab="",ylim=ylm,yaxt="n");
w7=pretty(exp(x),n=6)
axis(2,at=log(w7),lab=round(w7,1),srt=90,tcl=-.4,mgp=c(1,.5,0));
w8=pretty(x,n=6)
axis(4,at=w8,lab=round(w8,1),srt=90,tcl=-.4,mgp=c(1,.5,0));
mtext("Log Scale",side=4,line=1.6);
lnum=1.8;
}
mtext(paste("Test Level (",unit,")",sep=""),side=2,line=lnum);
mtext("Trial Number",side=1,line=2.2);

points(ens[rd],x[rd],pch=25,cex=.7,bg=4); 
points(ens[gr],x[gr],pch=24,cex=.7,bg=3);

if(test == 1) tf=add3pod(dtt,ylm);
if(test == 2) tf=addneyr(dtt,ylm);
if(test >  2) 
  {
  addBorL(dtt,ylm,ud); 
  if(length(en12)==2)
	{
	abline(v=en12[1]+.5,lty=3);
	xd1=dt$X[1:en12[1]]; yd1=dt$Y[1:en12[1]];
 		i10=which(yd1==0 & xd1==max(xd1[yd1==0])); i10=i10[1];
		i11=which(yd1==1 & xd1==min(xd1[yd1==1])); i11=i11[1];
	xd2=dt$X[en12[1]+1:en12[2]]; yd2=dt$Y[en12[1]+1:en12[2]];
		i20=which(yd2==0 & xd2==max(xd2[yd2==0])); i20=i20[1];
		i21=which(yd2==1 & xd2==min(xd2[yd2==1])); i21=i21[1];
	lines(c(i10,en12[1]+.5),rep(xd1[i10],2),col=3,lty=4);
	lines(c(i11,en12[1]+.5),rep(xd1[i11],2),col=4,lty=4);	
	lines(c(en12[1]+i20,.5+sum(en12)),rep(xd2[i20],2),col=3,lty=4);
	lines(c(en12[1]+i21,.5+sum(en12)),rep(xd2[i21],2),col=4,lty=4);
	}
  }
if(test == 2) {if(tf[1]) about=chabout(about,nrow(dtt),4);}

if(!notitle)
{
mtext(titl,side=3,line=3.4,cex=1.4);
mtext(about1,side=3,line=1.8,cex=1.3);
mtext(about,side=3,line=0.5,cex=1.3);
}

if(fini == 1)
{
axis(4,label=F,at=dt$RX[nid+1],tcl=.25,lwd=2); 	# Next EX had test cont'd (BL, Inside Box)
axis(4,label=F,at=zee,tcl=-.25,lwd=2);		# zee = pth quantile (BL, Outside Box)
}
reset();
}
}

pdat2=function(dat,notitle=F)
{
dt=dtt=dat$d0; about=dat$about; titl=dat$titl; ln=dat$ln; 
ttl0=dat$ttl0; ttl1=dat$ttl1; ttl2=dat$ttl2; test=dat$test; 
tnam=c("3pod","Neyer");
if(is.null(about)) {cat("This function only works for lists created by gonogo\n\n"); return();}
# pee & neyer aren't defined while running the test. Need to infer neyer.
pee=dat$p;
id=dt$ID; nid=length(id);
if(length(pee) == 0) pee=0;
fini=0; if(id[nid]=="III3") fini=1;
if(fini == 1) {dtt=dtt[-nid,]; id=id[-nid]; nid=nid-1;}
if(pee*(1-pee) > 0 & fini == 1) { yu=glmmle(dtt); zee=yu$mu+qnorm(pee)*yu$sig; }
about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[1],",",n[2],",",n[3],"|p,",lambda,",res}",sep=""));

kp=0; 
	for(j in 1:nid) 
	{	
	jj=m.update(dtt[1:j,]); 
	M0=jj$M0; m1=jj$m1; 
	uv=c(M0,m1); 
	if(!any(is.na(uv))) {if(M0 > m1) kp=j;}
	if(kp > 0) break;
	}
mus=sigs=zee=zee0=rep(0,nid-kp+1);

if(kp == 0) cat("ptest(z,plt=2) option requires having completed Phase I2 (i.e., achieving overlap)\n");
if(kp > 0)
{	
for(j in kp:nid) {g=glmmle(dtt[1:j,]); mus[j-kp+1]=g$mu; sigs[j-kp+1]=g$sig;}	
if(pee > 0 & pee < 1)zee=mus+qnorm(pee)*sigs;
par(mfrow=c(2,1), mar=c(1.5,2.5,.5,.5), oma=c(2,2,3.5,2));
lmu=pretty(mus); lsig=pretty(sigs); lx=pretty(c(kp,nid)); rx=kp:nid; rxx=range(rx);
if(diff(rxx)==0)rxx=rxx+c(-1,1)
plot(kp:nid,mus,type="l",xlab="",ylab="",xlim=rxx,xaxt="n",ylim=range(lmu),yaxt="n"); 
axis(1,at=kp:nid,labels=T,tck=-.03,mgp=c(1,.4,0),cex.axis=.8);
axis(2,at=lmu,labels=T,tck=-.02,mgp=c(1,.4,0),las=2,cex.axis=.8);
if(ln) mtext("Mean(Log)",side=2,line=3,cex=1) else mtext("Mean",side=2,line=3,cex=1);

lt=3; abline(h=lmu,lty=lt); abline(v=lx,lty=lt);
points(kp:nid,mus,pch=16,cex=.8);
if(kp == nid) nlx=2 else nlx=nid-kp;
plot(kp:nid,sigs,type="l",xlab="",ylab="",ylim=range(lsig),yaxt="n",xlim=rxx,xaxt="n"); 
axis(1,at=kp:nid,labels=T,tck=-.03,mgp=c(1,.4,0),cex.axis=.8);
axis(2,at=lsig,labels=T,tck=-.02,mgp=c(1,.4,0),las=2,cex.axis=.8);
mtext("Cumulative Test Size (n)",side=1,line=0,cex=1,outer=T);
if(ln) mtext("SD(Log)",side=2,line=3,cex=1) else mtext("SD",side=2,line=3,cex=1);

abline(h=lsig,lty=lt); abline(v=lx,lty=lt);	
points(kp:nid,sigs,pch=16,cex=.8);
par(mfrow=c(1,1));
els=c(2.5,1);
about8=paste(ttl0,", Sequence of MLE's",sep="");
if(!notitle) 
{
mtext(about8,side=3,line=2.8,cex=1.25);
if(test > 2){
mtext(ttl1,side=3,line=1.4,cex=1.1,adj=0);
mtext(ttl2,side=3,line=.3,cex=1.1,adj=0);
} else mtext(tnam[test],side=3,line=.3,cex=1.1,adj=0);
mtext(about1,side=3,line=1.4,cex=1.1,adj=1);
mtext(about,side=3,line=.3,cex=1.1,adj=1);
}
}
reset();
if(pee == 0) return(matrix(c(mus,sigs),ncol=2)) else return(matrix(c(mus,sigs,zee),ncol=3));
}

pdat3=function(w,notitle=F)
{
dt=dtt=w$d0; about=w$about; titl=w$titl; unit=w$unit; ln=w$ln; pee=w$p;
ttl0=w$ttl0; ttl1=w$ttl1; ttl2=w$ttl2; test=w$test; 
if(test > 4 | is.null(w$about)) {cat("This function only works for lists created by gonogo\n\n"); return();}

if(dev.cur() == 1)  plot.new();
blrb5();

xx="Enter conf and J: ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));
conf=xx[1]; J=xx[2];

cf=round(conf,4); cf=as.character(cf); cf=gsub("0.",".",cf,fixed=T);
pd=""; if(test == 3 | test == 4) pd=", ";
pcf=substitute(paste(ttl1,pd,c[],"=",cf,sep=""));

tnam=c("3pod","Neyer");
if(is.null(about)) {cat("This function only works for lists created by gonogo\n\n"); return();}
id=dt$ID; nid=length(id);
if(length(pee) == 0) pee=0;
fini=0; if(id[nid]=="III3") fini=1;
if(fini == 1) {dtt=dtt[-nid,]; id=id[-nid]; nid=nid-1;}
about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[1],",",n[2],",",n[3],"|p,",lambda,",res}",sep=""));

kp=0; 
	for(j in 1:nid) 
	{	
	jj=m.update(dtt[1:j,]); 
	M0=jj$M0; m1=jj$m1; 
	uv=c(M0,m1); 
	if(!any(is.na(uv))) {if(M0 > m1) kp=j;}
	if(kp > 0) break;
	}

kp=1;
val=sum(strwidth(unlist(strsplit(as.character(ttl0),split=""))))/strwidth("W");
ct=c(-Inf,16,17,21,25,Inf);
cx=c(1.25,1.15,1.05,.95,.85);
imx=max(which(ct < val));
sz=cx[imx]

if(kp > 0)
{
if(ln) z=qrda(dtt,conf,J,ln=T,labx=unit) else 
z=qrda(dtt,conf,J,labx=unit);
rmzm=xlead0(z$mu,3); rmzs=xlead0(z$sig,3);
about2=substitute(paste(ttl0,", (",hat(mu),",",hat(sigma),",n) = (",rmzm,",",rmzs,",",nid,")",sep=""));

if(!notitle)
{
mtext(about2,side=3,line=2.6,cex=sz);
sz2=1.1;
if(test > 2){
mtext(ttl2,side=3,line=.1,cex=sz2,adj=0);
} else mtext(tnam[test],side=3,line=.3,cex=sz2,adj=0);
mtext(pcf,side=3,line=1.4,cex=sz2,adj=0);
mtext(about1,side=3,line=1.4,cex=sz2,adj=1);
mtext(about,side=3,line=.3,cex=sz2,adj=1);
}
}
return(z)
}

intToBitVect = function(x)
{
# x is a vector of numbers
nx=length(x)
L=vector("list",nx)
for(i in 1:nx)
{
  xi=x[i]
  tmp = rev(as.numeric(intToBits(xi)))
  id = seq_len(match(1,tmp,length(tmp))-1)
  L[[i]]=tmp[-id]
}
return(L)
}

#INDEX 36 through 53, Xsim.R (18 functions)

gonogoSim=function(mlo,mhi,sg,n2=0,n3=0,p=0,lam=0,dm=0,ds=0,reso=0,ln=F,plt=0,iseed=-1,IIgo=T,M=1,test=1,BL=c(4,1,0))
{
dat0=data.frame(numeric(0));
dud=lev=NULL;
jvec=NULL;
transf=NULL;
if(M <= 0) M=1;
sgrem=sg=M*sg; mlo1=mlo=M*mlo; mhi1=mhi=M*mhi; dm=M*dm; ds=M*ds;
en12=numeric(0);

if(mlo == mhi & sg > 0) test=3;
if(mlo < mhi & sg == 0) test=4;

if(!is.element(test,1:4)) 	{vv="test must be 1,2,3 or 4. Try again\n"; cat(vv); return();}
if(test < 4) {if(sg <= 0) 	{vv="sg must be positive. Try again.\n"; cat(vv); return(); }}
if(mhi < mlo) 			{vv="mhi-mlo must be nonnegative. Try again.\n"; cat(vv); return(); }

if(test < 3)
{
	BL=NULL;
	del5=(mhi-mlo)/6;
	epsi=del5/1000;
	if(sg>(mhi-mlo)/6+epsi){cat(paste("sg is too big (sg <= ",round(del5,4),")\nTry again\n\n",sep="")); return();}
}
	
	if(!IIgo) {n2=n3=p=lam=0};

	if(n3 != 0 & (p <= 0 | p >= 1 | lam <= 0)) {cat(paste("p must be between 0 & 1 and lambda > 0.\nTry again\n\n",sep="")); return();}
	
	savinit=c(mlo,mhi,sg); 

if(ln){
	if(test < 3) { u=fgs(mlo,mhi,sg); mlo=u[1]; mhi=u[2]; sg=u[3]; }
	if(test == 3) {
	vv="For ln=T Bruceton, mlo (same as mhi) must be positive.\n";
	vv1="Also, sg must be between 0 and mlo/3. Try again.\n";
	if(mlo <= 0 | sg <= 0 | sg >= mlo/3) {cat(vv); cat(vv1); return();} else
	{mlo=log(mlo); mhi=log(mhi); sg=log(1+sg/mlo);}
	}
	if(test == 4) {
	vv="For ln=T Langlie, mlo < mhi and both must be positive. Try again.\n";
	if(mlo <= 0 | mhi <= 0 | mlo >= mhi) {cat(vv); return();} else {mlo=log(mlo); mhi=log(mhi);}
	}
	}

	init=c(mlo,mhi,sg);

if(test > 2) 
{
tx=paste(BL,collapse="");
if(all(BL[-1] == c(0,1)) | all(BL[-1] == c(1,1))) BL[2:3]=c(1,0);
I=BL[-1]; 
a=prtrans(I); dud=a$dud; lev=a$lev;
pm=c(1,-1); iz=which(I==0); lz=length(iz); 
if(lz == 1) I=I[-iz]; 
zp=zpfun(I); 
if(lz == 0)zp=c(0,1)+pm*zp;
if(lz == 1){if(iz == 1) zp=1-zp;} 
pchar=paste(xlead0(zp,4),collapse=",");
}

targmu=(mlo+mhi)/2;
targsig=sg;
if(test==4)targsig=(mhi-mlo)/6;

tmu=targmu+dm;
tsig=targsig+ds;

if (test == 1)
  {
	w=pI1(mlo,mhi,sg,tmu,tsig,reso,ln,iseed); 
	d0=w[[1]]; dat0=w[[2]]; 

	w=pI2(d0,dat0,sg,tmu,tsig,reso,ln,iseed); 
	d0=w[[1]]; dat0=w[[2]]; sg=w[[3]]; n12=0; n1=n11=nrow(d0);
	
	if(IIgo)
	{
	w=pI3(d0,dat0,sg,tmu,tsig,reso,ln,iseed);
	d0=w[[1]]; dat0=w[[2]]; n1=nrow(d0); n12=n1-n11;

		if(n2 < 0) n2=max(-(n2+n11+n12),0);
	if(n2 > 0) {w=pII(d0,dat0,tmu,tsig,n2,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]];}
		if(n3 < 0) n3=max(-(n3+n2+n11+n12),0);
	if(n3 > 0 & p*(1-p) > 0 & lam >= 0) {w=spIIIsim(d0,dat0,tmu,tsig,n3,p,lam,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]]; jvec=w[[3]];}
	}
  } 

if(test == 2)
  {
	w=npI(mlo,mhi,sg,tmu,tsig,reso,ln,iseed); 
	d0=w[[1]]; dat0=w[[2]]; n1=n11=nrow(d0); n12=0;	

	if(IIgo)
	{
		if(n2 < 0) n2=max(-(n2+n11+n12),0);
	if(n2 > 0) {w=pII(d0,dat0,tmu,tsig,n2,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]];}
		if(n3 < 0) n3=max(-(n3+n2+n11+n12),0);
	if(n3 > 0 & p*(1-p) > 0 & lam >= 0) {w=spIIIsim(d0,dat0,tmu,tsig,n3,p,lam,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]]; jvec=w[[3]];}
	}
  }

if(test > 2)
  {
	if(test == 3) w=bpI(mlo,mhi,sg,tmu,tsig,reso,ln,iseed,BL) else 
	w=lpI(mlo,mhi,sg,tmu,tsig,reso,ln,iseed,BL)
	d0=w[[1]]; dat0=w[[2]]; n1=n11=nrow(d0); n12=0;	
	en12=w[[3]]; n11=en12[1]; if(length(en12) > 1) n12=en12[2];

	if(IIgo)
	{
		if(n2 < 0) n2=max(-(n2+n11+n12),0);
	if(n2 > 0) {w=pII(d0,dat0,tmu,tsig,n2,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]];}
		if(n3 < 0) n3=max(-(n3+n2+n11+n12),0);
	if(n3 > 0 & p*(1-p) > 0 & lam >= 0) {w=spIIIsim(d0,dat0,tmu,tsig,n3,p,lam,reso,ln,iseed); d0=w[[1]]; dat0=w[[2]]; jvec=w[[3]];}
	}
  }

	en=c(n11,n12,n2,n3);
	v=glmmle(d0);
 
	abo=wabout13(M,mlo1,mhi1,sgrem,p,n11,n12,n2,n3,lam,reso);
	
	h1=""; if(ln) h1="Log ";

	titl=NULL;

	rmzm=round(targmu,3); if(abs(rmzm) < 1) rmzm=gsub("0.",".",as.character(rmzm),fixed=T); 
	rmzs=round(targsig,3); if(abs(rmzs) < 1) rmzs=gsub("0.",".",as.character(rmzs),fixed=T);
	rmdm=round(dm,3); if(abs(rmdm) < 1) rmdm=gsub("0.",".",as.character(rmdm),fixed=T);
	rmds=round(ds,3); if(abs(rmds) < 1) rmds=gsub("0.",".",as.character(rmds),fixed=T);

	if(iseed < 0) 
	{ttl0=substitute(paste("(",mu[t],", ",sigma[t],") = (",rmzm,", ",rmzs,") + (",rmdm,", ",rmds,")",sep=""));} else
	{ttl0=substitute(paste("(",mu[t],", ",sigma[t],") = (",rmzm,", ",rmzs,") + (",rmdm,", ",rmds,"), ",i[s]," = ",iseed,sep=""));}

	ttl1=ttl2=NULL; 
	if(ln) h2="Log " else h2=""; 
	if(test == 1) ttl2=paste(h2,"3pod",sep="");
	if(test == 2) ttl2=paste(h2,"Neyer",sep="");
	if(test == 3 | test == 4) 
	{
	if(test == 3) ttl1=substitute(paste(h2,Bruc[tx],sep=""));
	if(test == 4) ttl1=substitute(paste(h2,Lang[tx],sep=""));
	ttl2=substitute(paste(h2,L[pchar],sep=""));
	}
	
	if(M==1)uni="X" else uni=paste(M,"X",sep="");
	ret=list(d0,jvec,tmu,tsig,v$mu,v$sig,en,abo,titl,ttl1,ttl2,ttl0,uni,p,reso,ln,lam,test,M,dm,ds,iseed,BL,dud,lev);
	names(ret)=c("d0","jvec","tmu","tsig","mhat","shat","en","about","title","ttl1","ttl2","ttl0","units","p","reso",
	"ln","lam","test","M","dm","ds","iseed","BL","dud","lev")
	if(is.element(plt,c(1,2,3))) ptest(ret,plt);

	return(ret);
}

pI1=function(mlo,mhi,sg,tmu,tsig,reso,ln,iseed,dat0=data.frame(numeric(0)))
{
nret=c("d0","dat0");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;
del=(mhi-mlo)/6;
epsi=del/1000;
mi=c(mlo,mhi);
a=matrix(c(.75,.25,.25,.75),ncol=2,byrow=T);
xx=t(a%*%mi);
	
for(i in 1:2)	{u=gd0(xx[i],d0,dat0,"I1",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0; }

i1=0;
	
x=d0$X; y=d0$Y;
if(all(y==c(0,0)))
	{
		xx=mi[2];
		while(1)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=xx+1.5*i1*sg;
		u=gd0(xx,d0,dat0,"I1(i)",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0; 		
		if(d0$Y[nrow(d0)] == 1) break;
		}
	}		

if(all(y==c(1,1)))
	{
		xx=mi[1];
		while(1)
		{
		i1=i1+1;
		if(i1%%3 == 0) sg=2*sg;
		xx=xx-1.5*i1*sg;
		u=gd0(xx,d0,dat0,"I1(ii)",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
		if(d0$Y[nrow(d0)] == 0) break;
		}
	}
	
if(all(y==c(0,1))) d0$ID=rep("I1(iii)",length(y));

if(all(y==c(1,0)))
	{
		xx=c(mlo-3*sg,mhi+3*sg);
		for(i in 1:2)
			{
			u=gd0(xx[i],d0,dat0,"I1(iv)",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;
			}
	}	
ret=list(d0,dat0); 
names(ret)=nret; 
return(ret);
}

pI2=function(d0,dat0,sg,tmu,tsig,reso,ln,iseed)
{
nret=c("d0","dat0","sg");
idii="";
while(1)
	{
	# del = m1-M0; del < 0 ==> OVERLAP
	j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
	while(del >= 1.5*sg)
		{
		xx=(M0+m1)/2;
		id=paste(idii,"I2(ib)",sep="");
		u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0; 		
		j=m.update(d0); m1=j$m1; M0=j$M0; del=m1-M0;
		}
	if(del < 0) {ret=list(d0,dat0,sg); names=nret; return(ret);}
	j=n.update(d0); n0=j$n0; n1=j$n1;
	if(del >= 0) 
		{
		if(n0 > n1)
			{
			xx=m1+0.3*sg;
			id=paste(idii,"I2(ic)",sep="");
			u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
			if(d0$Y[nrow(d0)] == 0) {ret=list(d0,dat0,sg); names=nret; return(ret);}
			if(d0$Y[nrow(d0)] == 1)
				{
				xx=M0-.3*sg;	
				id=paste(idii,"I2(ic)",sep="");
				u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
				if(d0$Y[nrow(d0)] == 1) {ret=list(d0,dat0,sg); names=nret; return(ret);}
				if(d0$Y[nrow(d0)] == 0)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		if(n0 <= n1)
			{
			xx=M0-.3*sg;
			id=paste(idii,"I2(id)",sep="");
			u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
			if(d0$Y[nrow(d0)] == 1) {ret=list(d0,dat0,sg); names=nret; return(ret);}
			if(d0$Y[nrow(d0)] == 0)
				{
				xx=m1+.3*sg;	
				id=paste(idii,"I2(id)",sep="");
				u=gd0(xx,d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
				if(d0$Y[nrow(d0)] == 0) {ret=list(d0,dat0,sg); names=nret; return(ret);}
				if(d0$Y[nrow(d0)] == 1)
					{
					sg=2*sg/3;	
					idii=paste(idii,"r",sep="");	
					}
				}
			}
		}
}
ret=list(d0,dat0,sg); names(ret)=nret; return(ret);
}

pI3=function(d0,dat0,sg,tmu,tsig,reso,ln,iseed)
{
j=m.update(d0); M0=j$M0; m1=j$m1; del=m1-M0;
xx=(M0+m1)/2;
if(sg+del > 0)xx=xx+c(1,-1)*sg/2;
lxx=length(xx)
for(i in 1:lxx)
	{
	if(i < lxx) u=gd0(xx[i],d0,dat0,"I3",tmu,tsig,reso,ln,iseed); 
	if(i == lxx) u=gd0(xx[i],d0,dat0,"I3",tmu,tsig,reso,ln,iseed);
	d0=u$d0; dat0=u$dat0;
	}
ret=list(d0,dat0);
names(ret)=c("d0","dat0");		
return(ret);
}

npI=function(mlo,mhi,sg,tmu,tsig,reso,ln,iseed,dat0=data.frame(numeric(0)))
{
nret=c("d0","dat0");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6))); d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

del=(mhi-mlo)/6;
epsi=del/1000;

eps=1e-007
n=0;

bl=c("B0","B1","B2","B3","B4");

# lf: a flag to adjust X1 & X2 in the ln=T neyer case 
# which deviates a tad from a true neyer on the logs
# but allows X1 & X2 to stay the same in both ln modes

lf=0; 

while(1)
	{
	# PART 1 ************************************************************
	if(n == 0) block=0 else
		{
		j=n.update(d0); k0=j$n0; k1=j$n1;
		xlo=min(d0$X)
		xhi=max(d0$X)
		if(k1 <= eps) block=1 else
			{
			if(k1 >= n-eps) block=2 else
				{
				# PART 2 **************************************************
				j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
				dif = round(m1-M0,14)
				if(dif > sg) block=3 else 
					{
					if(dif >= 0) block=4 else block=5;
					}
				}
			}
		}

	# First
	if(block == 0) if(!ln)xbef = (mlo+mhi)/2 else {v=ifg(mlo,mhi); xbef=log((v[1]+v[2])/2); lf=1;}
	# All 0's
	if(block == 1) if(lf == 0) xbef = max(c((mhi + xhi)/2, xhi + 2 * sg, 2 * xhi - xlo)) else
	{xbef=log((v[1]+3*v[2])/4); lf=0;}
	# All 1's
	if(block == 2) if(lf == 0) xbef = min(c((mlo + xlo)/2, xlo - 2 * sg, 2 * xlo - xhi)) else
	{xbef=log((3*v[1]+v[2])/4); lf=0;}

	if(block == 3) xbef = (m1 + M0)/2
	if(block == 4) 
	{ 
	m=(m1+M0)/2; es=sg; sg=.8*sg;
	m = max(xlo, min(m, xhi))
	es = min(es,(xhi-xlo))
	b = yinfomat(d0,m,es)$infm
	xbef = m + kstar(b)*es
	}
	if(block == 5) break;
	u=gd0(xbef,d0,dat0,bl[block+1],tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;
	n=nrow(d0);		
}
ret=list(d0,dat0); 
names(ret)=nret; 
return(ret);
}

bpI=function(mlo,mhi,sg,tmu,tsig,reso,ln,iseed,BL,dat0=data.frame(numeric(0)))
{
nRev=BL[1];  I=BL[2:3]; I[I == 0] =-1;
X=c(1,0);
nSeq=1+(I-I%%2)/2;
wz=which(I == -1);
if(length(wz) == 1) {X=X[-wz]; I=I[-wz]; nSeq=nSeq[-wz];}
lox=length(X);
udid=numeric(0);

nret=c("d0","dat0","en12");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

en12=0; 

for(ijk in 1:lox)
{
if(X[ijk] == 0)GE5=F else GE5=T;

# L=intToBitVect(nL), L is a List, nl is a vector of integers
# D's are the first 1:nSeq, U's are the last 1 or 2 depending on nAdd being 0 or 1, resp.

L=udli(I[ijk]);
nL=bintodec(L);

pl=zpfun(I[ijk]);
if(!GE5)pl=1-pl;
xx=mlo;

# initialize counter and accumulator
icnt=iacc=0;
dn=0; ud=xud=numeric(0); yseq=numeric(0);

while(dn == 0)
{
u=gd0(xx,d0,dat0,"IB",tmu,tsig,reso,ln,iseed)

d0=u$d0; dat0=u$dat0;  
icnt=icnt+1; nx=length(d0$X); iacc=iacc+d0$X[[nx]];
y=d0$Y;
ny=length(y);

if(GE5) {yseq=c(yseq,y[ny]); udid=c(udid,y[ny]);} else 
{yseq=c(yseq,1-y[ny]); udid=c(udid,1-y[ny]);}

lseq=list(yseq); ns=bintodec(lseq);

iw=which(nL == ns);

# Also will want to know if overlap has been achieved
j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
dif = round(m1-M0,14);
if(!is.na(dif) & dif < 0 & nRev < 0) dn=1;

if(length(iw) == 1 & dn == 0)
  {
xx=iacc/icnt;
xud=c(xud,xx);
nxud=length(xud);
kay=floor((xx-mlo)/sg);
pstar=(xx-mlo)/sg-floor((xx-mlo)/sg);
g=mlo+kay*sg;

if(GE5)
{
if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=-2; if(pstar <= .5) xx=g-sg else xx=g;}
if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=2; if(pstar <= .5) xx=g+sg else xx=g+2*sg;}
} else
{
if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=2; if(pstar <= .5) xx=g+sg else xx=g+2*sg;}
if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=-2; if(pstar <= .5) xx=g-sg else xx=g;}
}
icnt=iacc=0;

  yseq=numeric(0);
  cud=sum(abs(diff(ud)));
  if(!is.na(dif) & nRev > 0) if(cud >= nRev & dif < 0) dn=1;
  if(!is.na(dif) & nRev == 0) if(dif < 0) dn=1;

# At this point, if dn = 1, then the reversal and overlap criteria are met
# Reset dn = 0 if Avg(X[Y==1]) <= Avg(X[Y==0]), i.e., if d.update(d0) < 1
  if(d.update(d0) < 1) dn=0;
  }
}
en12=c(en12,nx);
}
en12=diff(en12);
udid[abs(udid) != 2]=0;
udid[udid==-2]="D"; udid[udid==2]="U"; udid[udid == 0]="";
d0$ID=udid;
ret=list(d0,dat0,en12);
names(ret)=nret;
return(ret);
}

lpI=function(mlo,mhi,sg,tmu,tsig,reso,ln,iseed,BL,dat0=data.frame(numeric(0)))
{
nRev=BL[1];  I=BL[2:3]; I[I == 0] =-1;
X=c(1,0);
nSeq=1+(I-I%%2)/2;
wz=which(I == -1);
if(length(wz) == 1) {X=X[-wz]; I=I[-wz]; nSeq=nSeq[-wz];}
lox=length(X);
udid=numeric(0);

nret=c("d0","dat0","en12");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6)));d1=cbind(d1,"END"); 
d0=d1[-1,]; names(d0)=names(d1)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d0;

en12=0; 

for(ijk in 1:lox)
{
if(X[ijk] == 0)GE5=F else GE5=T;

# L=intToBitVect(nL), L is a List, nl is a vector of integers
# D's are the first 1:nSeq, U's are the last 1 or 2 depending on nAdd being 0 or 1, resp.

L=udli(I[ijk]);
nL=bintodec(L);

pl=zpfun(I[ijk]);
if(!GE5)pl=1-pl;
xx=mlo*(1-pl)+mhi*pl;

# initialize counter and accumulator
icnt=iacc=0;
dn=0; ud=xud=numeric(0); yseq=numeric(0);

while(dn == 0)
{
u=gd0(xx,d0,dat0,"IB",tmu,tsig,reso,ln,iseed)
d0=u$d0; dat0=u$dat0;  
icnt=icnt+1; nx=length(d0$X); iacc=iacc+d0$X[[nx]];

y=d0$Y;
ny=length(y);

if(GE5) {yseq=c(yseq,y[ny]); udid=c(udid,y[ny]);} else 
{yseq=c(yseq,1-y[ny]); udid=c(udid,1-y[ny]);}

lseq=list(yseq); ns=bintodec(lseq);

iw=which(nL == ns);

# Also will want to know if overlap has been achieved
j=m.update(d0); m1=j$m1; M0=j$M0; dif=m1-M0;
dif = round(m1-M0,14);
if(!is.na(dif) & dif < 0 & nRev < 0) dn=1;

  if(length(iw) == 1 & dn == 0)
  {
	if(GE5)
	{
	if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=-2;}
	if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=2;}
	} else
	{
	if(iw > nSeq[ijk]) {ud=c(ud,1); udid[ny]=2;}
	if(iw <= nSeq[ijk]) {ud=c(ud,0); udid[ny]=-2;}
	}

	xud=c(xud,iacc/icnt);
	nxud=length(xud);
	rud=2*rev(ud)-1;
	uc=cumsum(rud);
	ic=which(uc == 0);
	# Calculate xxa, xx, reset counters
	if(length(ic) == 0)
	{
		if(GE5)
  		{
  		if(iw > nSeq[ijk]) xxa=mlo;
  		if(iw <= nSeq[ijk]) xxa=mhi;
  		} else
  		{
  		if(iw > nSeq[ijk]) xxa=mhi;
  		if(iw <= nSeq[ijk]) xxa=mlo;
  		}
	}
  if(length(ic) > 0) {ic=ic[1]; xxa=rev(xud); xxa=xxa[ic];}
  xx=(xud[nxud]+xxa)/2;

  icnt=iacc=0;
  yseq=numeric(0);
  cud=sum(abs(diff(ud)));
  if(!is.na(dif) & nRev > 0) if(cud >= nRev & dif < 0) dn=1;
  if(!is.na(dif) & nRev == 0) if(dif < 0) dn=1;
# At this point, if dn = 1, then the reversal and overlap criteria are met
# Reset dn = 0 if Avg(X[Y==1]) <= Avg(X[Y==0]), i.e., if d.update(d0) < 1
  if(d.update(d0) < 1) dn=0;
  }
}
en12=c(en12,nx);
}
en12=diff(en12);
udid[abs(udid) != 2]=0;
udid[udid==-2]="D"; udid[udid==2]="U"; udid[udid == 0]="";
d0$ID=udid;

ret=list(d0,dat0,en12);
names(ret)=nret;
return(ret);
}

pII=function(d0,dat0,tmu,tsig,n2,reso,ln,iseed)
{
# Wu/Tian definition of n2 would be n2 = n2-nrow(d0);
xl=xu=xstar=mu2=mu4=sg2=sg4=rep(0,n2);
for(i in 1:n2)
	{
	nq=glmmle(d0);
	mu2[i]=nq$mu;
	sg2[i]=nq$sig;
	xl[i]=min(d0$X); xu[i]=max(d0$X);
	mu4[i]=max(xl[i],min(mu2[i],xu[i]));
	sg4[i]=min(sg2[i],xu[i]-xl[i]);
	b=yinfomat(d0,mu4[i],sg4[i])$infm;
	xstar[i]=mu4[i]+kstar(b)*sg4[i];
	id="II1";
	if(i > 1) id="II2";
	u=gd0(xstar[i],d0,dat0,id,tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;		
	}
ret=list(d0,dat0);
names(ret)=c("d0","dat0");		
return(ret);
}

spIIIsim=function(d0,dat0,tmu,tsig,n3,p,lam,reso,ln,iseed)
{
jvec=matrix(rep(0,10*(n3+1)),ncol=10);
nq=glmmle(d0);
mu=nq$mu; sig=nq$sig;

# Calculate initial tau1^2
# this variance/covariance matrix (vcov1) is scale free
ww=yinfomat(d0,mu,sig,solv=T);
tau2=sum(t(c(1,qnorm(p)^2))*diag(ww$vcov1));

# Truncate tau2[1]
ti=round((c(3,5)/qnorm(.975))^2,4)*sig^2;

#** NEW 
if(ln) ti=round((c(3,5)/qlnorm(.975))^2,4)*sig^2;

tau2=min(max(tau2[1],ti[1]),ti[2]);
	
# Use Mu Tilda and Sigma Tilda instead of Mu Hat and Sigma Hat 
m1=min(d0$X,na.rm=T); 
m2=max(d0$X,na.rm=T);
m2=min(c(mu,m2),na.rm=T);
mut=max(c(m1,m2),na.rm=T);
sigt=min(sig,diff(range(d0$X)),na.rm=T);
	
# Beta = 1/(2* SigmaTilda) per pp 9. You get beta = 0.4302985
be=1/(2*sigt);

#** NEW 
if(ln) be=plnorm(qlnorm(p))/(pnorm(qnorm(p))*sigt)

c1=f3point8(lam);
nu=sqrt(tau2)*c1;
	
xx=mut+qnorm(p)*sigt+nu;
u=gd0(xx,d0,dat0,"III1",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;
ny=length(d0$Y); yy=d0$Y[ny];
jvec[1,]=c(0,0,0,0,0,tau2,nu,0,xx,yy);
for(i in 1:n3)
{
	# Compute next X|d0
	vv=skewL(c1,nu,tau2,p,be);
	a=vv[5]; tau2=vv[6]; nu=vv[7]; b=vv[8];
	xx=d0$X[nrow(d0)]-a*(d0$Y[nrow(d0)]-b);

if(i < n3)
	{
	u=gd0(xx,d0,dat0,"III2",tmu,tsig,reso,ln,iseed); d0=u$d0; dat0=u$dat0;
	ny=length(d0$Y);
	yy=d0$Y[ny]
	jvec[i+1,]=c(vv,xx,yy);
	}
if(i == n3)
	{
	d0=rbind(d0,d0[nrow(d0),]);
	d0[nrow(d0),1:6]=c(0,0,0,round(xx,5),0,0);
	d0$ID[nrow(d0)]="III3";
	jvec[i+1,]=c(vv,xx,NA);
	}
}
jvec=data.frame(jvec);
names(jvec)=c("j","k","v","u","a","tau2","nu","b","x","y");
ret=list(d0,dat0,jvec); 
names(ret)=c("d0","dat0","jvec");
return(ret);
}

gd0=function(xx,d0,dat0,ID,mu,sig,reso,ln,iseed=-1)
{
nret=c("d0","dat0");
cnam=c("X","Y","COUNT","RX","EX","TX","ID");
d1=data.frame(t(rep(0,6))); d1=cbind(d1,"END"); 
names(d1)=names(d0)=cnam;
d0$ID=as.character(d0$ID);
if(is.null(dat0)) dat0=d1[-1,];
n0=nrow(dat0); nd0=nrow(d0)+1;
iset=0; if(iseed >= 0)iset=nd0+iseed;
if(n0 == 0)
	{
	u=gxr(xx,mu,sig,reso,ln,iset);
	d1[1,1:6]=c(u[1:2],1,u[3],xx,u[4]); d1$ID=ID;
	} 	
if(n0 > 0)	{d1=dat0[1,]; dat0=dat0[-1,]; if(is.null(dat0)) dat0=d1[-1,]; n0=nrow(dat0);}
d0=rbind(d0,d1);
ret=list(d0,dat0);	
names(ret)=nret;
return(ret);
}

gxr=function(x,mu,sig,reso,ln,iset)
{
if(iset > 0) set.seed(iset);
xsav=x;
if(ln) x=exp(x);
rx=round(x,6); if(reso > 0)rx=round(x/reso)*reso;
# rx=the stress; Choose a random strength (stren, that is NOT on log scale) 
if(ln) stren=rlnorm(1, meanlog = mu, sdlog = sig) else stren=mu+rnorm(1)*sig;
# If stren is bigger than rx (stress) then r=0; 
r=0; if(stren <= rx) r=1;
x=tx=round(rx,5);
if(ln) x=round(log(tx),5)
return(c(x,r,rx,tx));
}

ntau = function(dat,response=1)
{
	# NOTE: ntau(dat,response=0) == -ntau(dat,response=1)
	# Whatever the response(i.e., 0 or 1) with Pr[response increases as stress increases] 
	# then a positive ntau(dat) <---> to a proper non-decreasing CDF response model
	# a negative ntau <---> to a flat response model <---> Mu=-Inf, Sig=Inf, pnorm((X-Mu)/Sig)=C
	# where C=#responses/#tested = r/n, say. then (X-MU)/Sig=qnorm(r/n) for all X ==> Sig=K, Mu=X-K
	# For K = Inf (i.e., K very large).
	
	if(response==0) dat$Y=1-dat$Y;
	st=dat$X;	
	i1=which(dat$Y==1); 
	i0=which(dat$Y==0); 
	r1=r0=n=dat$COUNT;
	r1[i0]=0; 
	r0[i1]=0;	
	nt=sum(n); n1=sum(r1); 
	tau1=sum((r1/n-n1/nt)*(n*st))
	tau2=sum(r1*st)-n1*sum(n*st)/nt
	tau3=sum(r1*st)-n1*mean(st,weights=n)
	tau4=n1*(mean(st,weights=r1)-mean(st,weights=n))
	return(tau4)
}

wabout13=function(M,cmlo,cmhi,csg,p,n11,n12,n2,n3,lam,reso)
{
g=", "; f=",";
cp=as.character(p); cp=gsub("0.",".",cp,fixed=T); 
cl=as.character(lam); cl=gsub("0.",".",cl,fixed=T); 
cr=as.character(reso); cr=gsub("0.",".",cr,fixed=T);
cmlo=as.character(cmlo); cmlo=gsub("0.",".",cmlo,fixed=T);
cmhi=as.character(cmhi); cmhi=gsub("0.",".",cmhi,fixed=T);
csg=as.character(csg); csg=gsub("0.",".",csg,fixed=T);
if(M == 1) 
{
if(n3 == 0) about=paste("{",cmlo,f,cmhi,f,csg,"|",n11,f,n12,f,n2,f,n3,"|","-",f,"-",f,cr,"}",sep="") else
about=paste("{",cmlo,f,cmhi,f,csg,"|",n11,f,n12,f,n2,f,n3,"|",cp,f,cl,f,cr,"}",sep="")
} else 
{
if(n3 == 0) about=paste("{",cmlo,f,cmhi,f,csg,"|",n11,f,n12,f,n2,f,n3,"|","-",f,"-",f,cr,f,M,"}",sep="") else
about=paste("{",cmlo,f,cmhi,f,csg,"|",n11,f,n12,f,n2,f,n3,"|",cp,f,cl,f,cr,f,M,"}",sep="")
}
return(about)
}

pSdat1=function(dat,notitle=F,ud=F)
{
dt=dtt=dat$d0; about=dat$about; titl=dat$titl; unit=dat$unit; pee=dat$p;
ln=dat$ln; test=dat$test; tmu=dat$tmu; tsig=dat$tsig; lev=dat$lev;
ttl1=dat$ttl1; ttl2=dat$ttl2; ttl0=dat$ttl0; 
res=dat$reso;
M=dat$M; dm=dat$dm; ds=dat$ds; iseed=dat$iseed; en=dat$en;
en12=en[1:2];

if(length(pee) == 0) pee=0;
x=dt$X; y=dt$Y; id=dt$ID; nid=length(id);

fini=0; if(id[nid]=="III3") fini=1;
if(fini == 1) {dtt=dtt[-nid,]; x=x[-nid]; y=y[-nid]; id=id[-nid]; nid=nid-1;}
zee=tzee=x[1]; 
if(pee*(1-pee) > 0 & fini == 1)
{
yu=glmmle(dtt); zee=yu$mu+qnorm(pee)*yu$sig;
tzee=dat$tmu+qnorm(pee)*dat$tsig;
}

# orig units were (mlo,mhi,sg). new units are (mlo,mhi,sg)*M.   
# To get X's, zee and tzee back into original units divide them by M 
# x=x/M; zee=zee/M; tzee=tzee/M;

if(M == 1) about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res}",sep="")) else
about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res,M}",sep=""));

ens=1:nid; rd=which(y==1); gr=which(y==0); 
xtz=c(x,tzee,zee);
ylm=range(pretty(c(xtz,max(xtz,na.rm=T)+diff(range(xtz))/80),n=10));

# for tick locations 
lb=nid-1; if(lb > 30) lb=ceiling(lb/2);

if(nid == 1) return();

if(nid > 1)
{
par(mar=c(4,4,5,2) + 0.1);
lnum=2.3;
if(!ln)plot(c(ens,1),c(x,zee),type="n",xlab="",ylab="",ylim=ylm,lab=c(lb,5,7)) else
{
par(mar=c(4,3,5,3) + 0.1);
plot(c(ens,1),c(x,zee),type="n",xlab="",ylab="",ylim=ylm,yaxt="n");
w7=pretty(exp(x),n=6)
axis(2,at=log(w7),lab=round(w7,1),srt=90,tcl=-.4,mgp=c(1,.5,0));
w8=pretty(x,n=6)
axis(4,at=w8,lab=round(w8,1),srt=90,tcl=-.4,mgp=c(1,.5,0));
mtext("Log Scale",side=4,line=1.6);
lnum=1.8;
}
mtext(paste("Test Level (",unit,")",sep=""),side=2,line=lnum);
mtext("Trial Number",side=1,line=2.2);
points(ens[rd],x[rd],pch=25,cex=.7,bg=4); 
points(ens[gr],x[gr],pch=24,cex=.7,bg=3);

if(test == 1) g7=add3pod(dtt,ylm,sim=T);
if(test == 2) g7=addneyr(dtt,ylm,sim=T);
if(test > 2) g7=addBorL(dtt,ylm,ud);
if(test < 3) kp=g7[2];
if(test > 2)
{
text((en[1]+en[2])/2+.5,ylm[2],"I",cex=.9);
if(en[3] > 0 & en[2] == 0) text(en[1]+en[3]/2+.5,ylm[2],"II",cex=.9);
if(en[4] > 0 & en[2] == 0) text(en[1]+en[3]+en[4]/2+.5,ylm[2],"III",cex=.9);
}

if(test >  2) 
  {
  if(length(en12)==2)
	{
	abline(v=en12[1]+.5,lty=3);
	xd1=dt$X[1:en12[1]]; yd1=dt$Y[1:en12[1]];
 		i10=which(yd1==0 & xd1==max(xd1[yd1==0])); i10=i10[1];
		i11=which(yd1==1 & xd1==min(xd1[yd1==1])); i11=i11[1];
	xd2=dt$X[en12[1]+1:en12[2]]; yd2=dt$Y[en12[1]+1:en12[2]];
		i20=which(yd2==0 & xd2==max(xd2[yd2==0])); i20=i20[1];
		i21=which(yd2==1 & xd2==min(xd2[yd2==1])); i21=i21[1];
#	lines(c(i10,en12[1]+.5),rep(xd1[i10],2),col=3,lty=4);
#	lines(c(i11,en12[1]+.5),rep(xd1[i11],2),col=4,lty=4);	
#	lines(c(en12[1]+i20,.5+sum(en12)),rep(xd2[i20],2),col=3,lty=4);
#	lines(c(en12[1]+i21,.5+sum(en12)),rep(xd2[i21],2),col=4,lty=4);
	}
}

a0=1;
sz=1.4; 
sz1=1.3;

if(!notitle)
{
if(test > 2) mtext(ttl1,side=3,line=1.8,cex=sz1,adj=0);
mtext(ttl2,side=3,line=0.5,cex=sz1,adj=0);
mtext(ttl0,side=3,line=3.4,cex=sz);
mtext(about1,side=3,line=1.8,cex=sz1,adj=a0);
mtext(about,side=3,line=0.5,cex=sz1,adj=a0); 
if(en[2] > 0) abline(v=en[1]+.5,lty=3)
}

if(fini == 1)
{
axis(4,label=F,at=dt$RX[nid+1],tcl=.25,lwd=2);	# Next EX had test cont'd (BL, Inside Box)
axis(4,label=F,at=zee,tcl=-.25,lwd=2);		# zee = pth quantile (BL, Outside Box)
axis(4,label=F,at=tzee,tcl=-.25,lwd=2,col=8);	# zee = pth quantile
axis(4,label=F,at=tzee,tcl=.25,lwd=2,col=8);	# zee = pth quantile
}
}
reset();
return();
}

pSdat2=function(dat,notitle=F)
{
dt=dtt=dat$d0; about=dat$about; titl=dat$titl; pee=dat$p;
ln=dat$ln; iseed=dat$iseed; lev=dat$lev;
ttl1=dat$ttl1; ttl2=dat$ttl2; ttl0=dat$ttl0; test=dat$test;
tmu=dat$tmu; tsig=dat$tsig;
M=dat$M; dm=dat$dm; ds=dat$ds;

if(length(pee) == 0) pee=0;
id=dt$ID; nid=length(id);
fini=0; if(id[nid]=="III3") fini=1;
if(fini == 1) {dtt=dtt[-nid,]; id=id[-nid]; nid=nid-1;}

if(M == 1) about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res}",sep="")) else
about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res,M}",sep=""));

kp=0; 
        for(j in 1:nid) 
        {       
        jj=m.update(dtt[1:j,]); 
        M0=jj$M0; m1=jj$m1; 
        uv=c(M0,m1); 
        if(!any(is.na(uv))) {if(M0 > m1) kp=j;}
        if(kp > 0) break;
        }

	mus=sigs=zee=rep(0,nid-kp+1);

if(kp == 0) cat("ptest(z,plt=2) option requires having completed Phase I2 (i.e., achieving overlap)\n");
if(kp > 0)
{       
for(j in kp:nid) {g=glmmle(dtt[1:j,]); mus[j-kp+1]=g$mu; sigs[j-kp+1]=g$sig;}   
if(pee > 0 & pee < 1)zee=mus+qnorm(pee)*sigs;
par(mfrow=c(2,1), mar=c(1.5,2.5,.5,.5), oma=c(2,2,3.5,2));
lmu=pretty(mus); lsig=pretty(sigs); lx=pretty(c(kp,nid)); rx=kp:nid; rxx=range(rx);
if(diff(rxx)==0)rxx=rxx+c(-1,1)
plot(kp:nid,mus,type="l",xlab="",ylab="",xlim=rxx,xaxt="n",ylim=range(lmu),yaxt="n"); 
axis(1,at=kp:nid,labels=T,tck=-.03,mgp=c(1,.4,0),cex.axis=.8);
axis(2,at=lmu,labels=T,tck=-.02,mgp=c(1,.4,0),las=2,cex.axis=.8);
if(ln) mtext("Mean(Log)",side=2,line=3,cex=1) else mtext("Mean",side=2,line=3,cex=1);

lt=3; abline(h=lmu,lty=lt); abline(v=lx,lty=lt);
points(kp:nid,mus,pch=16,cex=.8);
if(kp == nid) nlx=2 else nlx=nid-kp;
plot(kp:nid,sigs,type="l",xlab="",ylab="",ylim=range(lsig),yaxt="n",xlim=rxx,xaxt="n"); 
axis(1,at=kp:nid,labels=T,tck=-.03,mgp=c(1,.4,0),cex.axis=.8);
axis(2,at=lsig,labels=T,tck=-.02,mgp=c(1,.4,0),las=2,cex.axis=.8);
mtext("Cumulative Test Size (n)",side=1,line=0,cex=1,outer=T);
if(ln) mtext("SD(Log)",side=2,line=3,cex=1) else mtext("SD",side=2,line=3,cex=1);

abline(h=lsig,lty=lt); abline(v=lx,lty=lt);     
points(kp:nid,sigs,pch=16,cex=.8);
par(mfrow=c(1,1));
els=c(2.5,1);

if(!notitle)
{
mtext(ttl0,line=2.7,cex=1.25);
mtext(ttl1,side=3,line=1.5,cex=1.1,adj=0);
mtext(about1,side=3,line=1.4,cex=1.1,adj=1);
mtext(ttl2,side=3,line=.3,cex=1.1,adj=0);
mtext(about,side=3,line=.3,cex=1.1,adj=1);
}

}
reset();
return(matrix(c(mus,sigs,zee),ncol=3));
}

pSdat3=function(dat,notitle=F)
{
dt=dtt=dat$d0; about=dat$about; titl=dat$titl; unit=dat$unit;
ln=dat$ln; iseed=dat$iseed; lev=dat$lev; test=dat$test;
tmu=dat$tmu; tsig=dat$tsig;
M=dat$M; dm=dat$dm; ds=dat$ds;
ttl1=dat$ttl1; ttl2=dat$ttl2; ttl0=dat$ttl0;
id=dt$ID; nid=length(id);
fini=0; if(id[nid]=="III3") fini=1;
if(fini == 1) {dtt=dtt[-nid,]; nid=nid-1;}

if(M == 1)about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res}",sep="")) else
about1=expression(paste("{",mu[lo],",",mu[hi],",",sigma[g],"|",n[11],",",n[12],",",n[2],",",n[3],"|p,",lambda,",res,M}",sep=""));

kp=0; 
        for(j in 1:nid) 
        {       
        jj=m.update(dtt[1:j,]); 
        M0=jj$M0; m1=jj$m1; 
        uv=c(M0,m1); 
        if(!any(is.na(uv))) {if(M0 > m1) kp=j;}
        if(kp > 0) break;
        }

if(kp == 0) cat("ptest(z,plt=3) option requires having completed Phase I2 (i.e., achieving overlap)\n");
if(kp > 0)
{
blrb5();

xx="Enter conf and J: ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));
conf=xx[1]; J=xx[2];
cf=round(conf,4); cf=as.character(cf); cf=gsub("0.",".",cf,fixed=T);
pcf=substitute(paste(c[],"=",cf,sep=""));

if(ln) z=qrda(dtt,conf,J,ln=T,labx=unit) else 
z=qrda(dtt,conf,J,labx=unit);
r1=.5; if(test == 3 | test == 4) r1=1;
ntx=substitute(paste(ttl1,", ",pcf,sep=""));

if(!notitle)
{
if(test == 3 | test == 4)mtext(ntx,side=3,line=1.6,cex=1.1,adj=0) else
mtext(pcf,side=3,line=1.5,cex=1.1,adj=0)
mtext(ttl0,side=3,line=2.8,cex=1.25);
mtext(about1,side=3,line=1.4,cex=1.1,adj=1);
mtext(ttl2,side=3,line=.2,cex=1.1,adj=0);
mtext(about,side=3,line=.2,cex=1.1,adj=1);
}

}
return(z)
}

# NEXT TWO TO REPRODUCE FIGURE 4 IN CHANCE ARTICLE

nmel3=function(mu,sg,conf,nt,ns,isd0,test=1,targp=.5,meth=3,fixx=T,inum=-1,icirc=numeric(0))
{

# Offsets: dm=ds=0
# Sigma=Sigma True, Vnom=True Mu
# mlo=Vnom-4*Sigma, mhi=Vnom+4*Sigma, sg=Sigma (Scaled by 10)
# True Mu = (mlo+mhi)/2
# True Sig = sg
# nt = n test

t0=proc.time()
mnfv=mass=numeric(0);
fail=fMASS=fMNFV=fBOTH=0;
k=4; v0=50; #(CHANCE Article)
i=0
savtest=NULL
em=es=ts=cee=numeric(0)
gnum=numeric(0)

while(i < ns)
{
i=i+1
isd=isd0+nt*(i-1);
mlo=mu-k*sg;
mhi=mu+k*sg;
if(test==2)g=gonogoSim(mlo,mhi,sg,n2=--nt,test=2,iseed=isd) else
g=gonogoSim(mlo,mhi,sg,n2=-nt,test=1,iseed=isd)
#if(i == 1431) {savtest=g}
if(length(icirc) == 1) { if(i == icirc) savtest=g; }

if(i == inum) gnum=g
em=c(em,g$mhat)
es=c(es,g$shat)
u=lims(meth,g$d0,conf,P=c(.001,.000001))

mnfv=c(mnfv,u[1,1])
mass=c(mass,u[2,2])

if(u[1,1] <= v0) m1=1 else m1=0
if(u[2,2] <= v0) m2=1 else m2=0

fMNFV=fMNFV+m1;
fMASS=fMASS+m2;
fBOTH=fBOTH+m1*m2;
eff=m1+m2-m1*m2;
fail=fail+eff;
cee=c(cee,1+eff)
ts=c(ts,1-fail/i)
if(i == ns) break
}
pass=1-fail/i
t1=proc.time()-t0
t1=as.numeric(t1[3])
w=which(cee == 2)
print(round(c(t1,ns,i,mu,sg,pass),6));

mmm=list(nt=nt,isd0=isd0,mu=mu,sg=sg,nsim=ns,i=i,fMNFV=fMNFV,fMASS=fMASS,
fBOTH=fBOTH,Ppass=pass,t1=t1,em=em,es=es,w=w,conf=conf,inum=inum,
gnum=gnum,ts=ts,meth=meth,mnfv=mnfv,mass=mass,targp=targp,savtest=savtest)

if(length(icirc) == 1) plotmm(mmm,icirc) else plotmm(mmm)

return(mmm)
}

plotmm=function(mm,icirc=numeric(0))
{
w=mm$w
fMNFV=mm$fMNFV
fBOTH=mm$fBOTH
fMASS=mm$fMASS
ii=mm$i
mass=mm$mass
mnfv=mm$mnfv
sg=mm$sg
mu=mm$mu
isd0=mm$isd0
nt=mm$nt

aa=paste("Counts per Quadrant Number (1, 2, 3, 4) = (",ii+fBOTH-fMNFV-fMASS,", ",fMNFV-fBOTH,", ",fBOTH,", ",fMASS-fBOTH,")",sep="")
t3=ii-(fMNFV+fMASS-fBOTH);
t4=round(t3/ii,3)
t1=round(10*mu,3); t2=round(10*sg,3); ti=mm$isd0;
bb=substitute(paste("2000 Simulations with ",V[nom]," = ",t1,", ",sigma[nom]," = ",t2,", ",i[seed[0]]," = ",ti,sep=""));
bbb=substitute(paste("Figure 4. Pr [Qualifying ] = ",t3," / ",ii," = ",t4,sep=""));

plot(10*mnfv,10*mass,pch=16,cex=.4,xlab="",ylab="",type="n")
points(10*mnfv[-w],10*mass[-w],pch=16,cex=.4,col=1)
points(10*mnfv[w],10*mass[w],pch=16,cex=.4,col=1)
mtext("MNFV",side=1,line=2.3)
mtext("MASS",side=2,line=2.3)
mtext(bb,side=3,line=1.6,cex=1)
mtext(bbb,side=1,line=3.5,cex=.9)
mtext(aa,side=3,line=.6,cex=1)

abline(v=500,lty=3)
abline(h=500,lty=3)

if(length(icirc == 1))
{
points(10*mm$mnfv[icirc],10*mm$mass[icirc],cex=1.2)
ti1=paste("Test ",isd0+nt*(icirc-1),": ",sep="")
text(432,643,ti1,cex=1)
points(470,641,pch=16,cex=.4)
points(470,641,cex=1.2)
lines(c(389,481),c(632,632),lty=1)
ti2=paste("MNFV = ",round(10*mm$mnfv[icirc],4),sep="")
ti3=paste("MASS = ",round(10*mm$mass[icirc],4),sep="")
text(436,623,ti2,cex=.9)
text(436,608,ti3,cex=.9)
}

return()
}

#INDEX 54-96, XComm (41 functions plus two constants)

skewL=function(c1,nu,tau2,p,be)
{
	# tau2 = the square of tau
	# nu = sqrt(tau2)*c1 where c1 = f3point8(lambda) (solving 3.8)
	# Compute a by 3.9 & 3.12 (below	u=E(Zn*M(Zn)) & v=E(M(Zn))	)
		
		j=qnorm(p)+be*nu;
		k=sqrt(1+be^2*tau2);
		v=pnorm(j/k);
		u=be*tau2*dnorm(j/k)/k+nu*v;
		a=(u-nu*v)/(v*(1-v));

	# Compute next tau2 by 3.4 & 3.12

		ntau2=a^2*v*(1-v)-2*a*(u-nu*v)+tau2;

	# Compute next nu

		nnu=sqrt(ntau2)*c1;

	# Compute next b by 3.10 & 3.12
	
		b=v-(nu-nnu)/a;

	return(c(j,k,v,u,a,ntau2,nnu,b))
}

n.update=function(dat)
{
n1=sum(dat[,2]);
n0=ncol(t(dat))-n1;
ret=list(n0,n1);
names(ret)=c("n0","n1");
return(ret);
}

m.update=function(dat)
{
n1=sum(dat[,2]);
n0=ncol(t(dat))-n1;
M0=m1=NA
if(n0 > 0) M0=max(dat[dat[,2]==0,1],na.rm=T);
if(n1 > 0) m1=min(dat[dat[,2]==1,1],na.rm=T);
ret=list(M0,m1);
names(ret)=c("M0","m1");
return(ret);
}

glmmle = function(mydata)
{
	mydata=na.omit(mydata);
	x = rep(mydata$X, mydata$COUNT);
	y = rep(mydata$Y, mydata$COUNT);
	n=length(x);
	options(warn = -1); 
	nmxx=c("anom", "mix", "one23", "mu", "sig", "maxll", "maxlc");
		
	j=m.update(mydata); M0=j$M0; m1=j$m1; 
	del=m1-M0; one23=2+sign(del);
	
	anom=T; mix=F;
	if(all(y==1) | all(y==0))
	{
		mu=NA; sig=NA; ll=NA; lcon=0;
		xx=list(anom, mix, one23, mu, sig, ll, lcon); 
		names(xx)=nmxx;
		return(xx);
	}
	
	mix=T; mu=(m1+M0)/2; sig=0;
	n1=sum(y); lcon=n1*log(n1/n)+(n-n1)*log(1-n1/n);

	if(one23 == 3) 
		{
		ll=0;
		xx=list(anom, mix, one23, mu, sig, ll, lcon); 
		names(xx)=nmxx;
		return(xx);
		}
		
	if(one23 == 2) 
		{
		i0=length(which(x[y==0]==M0)); 	
		i1=length(which(x[y==1]==M0));
		p1=i1/(i0+i1); ll=i1*log(p1)+i0*log(1-p1);
		xx=list(anom, mix, one23, mu, sig, ll, lcon); 
		names(xx)=nmxx;
		return(xx);
		}
	
	u=tauf(x,y); tau=u$tau; p1=u$p1;
	
	if(tau <= 0) 
		{
		mu=-Inf; sig=Inf; 
		n1=sum(y); ll=n1*log(n1/n)+(n-n1)*log(1-n1/n);
		xx=list(anom, mix, one23, mu, sig, ll, lcon); 
		names(xx)=nmxx;
		return(xx);
		}

	# one23 = 1 & tauf(x,y) > 0

	anom=F;
	xglm = glm(y ~ x, family = binomial(link = probit))
	ab = as.vector(xglm$coef);
	mu= -ab[1]/ab[2]; sig=1/ab[2]; ll=llik(mydata,mu,sig);
	xx=list(anom, mix, one23, mu, sig, ll, lcon); 
	names(xx)=nmxx;
	return(xx);
}

llik = function(mydata, mu, sig)
{
	# Remove rows of data having NA's in them
	mydata=na.omit(mydata)
	x = mydata$X
	y = mydata$Y
	n = mydata$COUNT
	i1 = which(y == 1)
	eps = 1e-006
	if(sig < eps) sig = eps
	ll = n[i1] * log(pnorm((x[i1] - mu)/sig))
	ll = c(ll, n[ - i1] * log(1 - pnorm((x[ - i1] - mu)/sig)))
	a=sum(ll); if(is.na(a))a=-Inf;
	return(a)
}

tauf = function(x,y)
{
	# See A. B. OWEN and P. A. ROEDIGER, The Sign of the Logistic Regression Coefficient, 
	# The American Statistician, November 2014, Vol. 68, No. 4, pp 297 - 301
	
	st=x;	
	i1=which(y==1); 
	i0=which(y==0); 
	r1=r0=n=rep(1,length(x));
	r1[i0]=0; 
	r0[i1]=0;	
	nt=sum(n); n1=sum(r1); n0=sum(r0);
	# tau1=sum((r1/n-n1/nt)*(n*st)); tau2=sum(r1*st)-n1*sum(n*st)/nt; 
	# tau3=sum(r1*st)-n1*weighted.mean(st,n);
	# tau4=n1*(weighted.mean(st,r1)-weighted.mean(st,n)); 
	tau5=n0*n1*(weighted.mean(st,r1)-weighted.mean(st,r0))/(n0+n1);
	xx=list(tau5,n1/nt);
	names(xx)=c("tau","p1");
	return(xx)
}

# yinfomat returns a scale-free (SF) version of infm, vcov and deti
# yqrda has been adjusted accordingly:
# actual infm = SF infm X sig^2; 	actual vcov = SF vcov / sig^2
# actual deti = SF deti / sig^4; 	(Note:  actual  rho = SF rho)

yinfomat = function(dat, mu, sig, solv=F)
{
	n = dat$COUNT
	k = (dat$X - mu)/sig
	p = pnorm(k) * (1 -pnorm(k))
	
	z = dnorm(k)
	v = n*z^2/p
	v[which(v == Inf)]=0
	
	# z, e.g., z = 4.881666e-226, is s.t. z^2 == 0 exactly
	# so that, when p == 0 exactly, v = 0/0 = NA
	# Therefore, have to remove the NA's if there are any
	iy=which(is.na(v))
	if(length(iy) > 0){v=v[-iy]; k=k[-iy];}
	
	b11= sum(v)
	b21 = b12 = sum(v*k)
	b22 = sum(v*k^2)
	
	# FISHER INFORMATION MATRIX
	infm = matrix(c(b11, b12, b21, b22), nrow = 2, byrow = T)
	# DETERMINANT OF INFORMATION MATRIX
	deti = det(infm)
vcov1=rho=NULL
if(solv)
{
	# VARIANCE COVARIANCE MATRIX
	vcov1 = solve(infm)
	# CORRELATION COEFFICIENT
	rho=vcov1[1,2]/sqrt(vcov1[1,1]*vcov1[2,2]);
}
	xx=list(vcov1,infm,deti,rho);
	names(xx)=c("vcov1","infm","deti","rho");
	return(xx)
}

Sk=function(k,b)
{
# b is the information matrix
v=b[1,1]*k^2-2.0*b[1,2]*k+b[2,2]
return(v)
}

Gk=function(k)
{
pk=pnorm(k)
gk=dnorm(k)/sqrt(pk*(1-pk))
return(gk^2)
}

dgs=function(k,b)
{
# derrivative of g^2*s, where g=dk/sqrt(pk*(1-pk))
# s=b11*k^2-2*b12*k+b22, b=Information Matrix
pk=pnorm(k)
dk=dnorm(k)
sk=b[1,1]*k^2-2*b[1,2]*k+b[2,2]
j=2*(b[1,1]-sk)*k-(2*b[1,2]+dk*sk*(1-2*pk)/(pk*(1-pk)))
return(j)
}

kstar=function(b)
{
# b=information matrix; presumption: kmax & b12 have opposite signs
# first part finds [l2,0] or [0,l2] containing the max g(k)^2*s(k)
# second part solves for the zero of d/dk of g(k)^2*s(k)
# this function uses functions Sk, Gk and dgs
del=-1
k1=0
val1=Gk(k1)*Sk(k1,b)
val2=val1+1
# Riley's Data made val1 < 1e-33 ! 1e-8 set (arbitrarily) avoid this anomalous case
valmin1=1e-8
if(val1 < valmin1) val1=valmin1
if(b[1,2] <= 0)del=1
while(val2 > val1)
{
k2=k1+del
val2=Gk(k2)*Sk(k2,b)
k1=k2
}
eps=.000001
k1=0
v1=dgs(k1,b)
v2=dgs(k2,b)
while(abs(k2-k1) > eps | abs(v2-v1) > eps)
{
kmid=(k1+k2)/2
vmid=dgs(kmid,b)
if(v1*vmid > 0){v1=vmid; k1=kmid;} else {v2=vmid; k2=kmid;}
}
kmax=(k1+k2)/2
return(kmax)
}

pavdf=function(data.df, ln, plotit = F, lineit = F, labx = "STIMULUS", laby = 
	"PROBABILITY OF RESPONSE", titl = "PAV SOLUTION")
{
	# 
	#	FUNCTION TO COMPUTE AND PLOT POOLED ADJACENT VIOLATORS (PAV) ALGORITHM
	#	responses are assumed to be 1 where Pr[Y=1] increases as the stress increases 
	#	data.df is a 3 Column dataframe who's Column Names are:
	#	X=Stresses, Y=Responses, COUNT=Number of Y's per X, respectively.  
	#	RETURNS list with components:	$full,   matrix with a number of rows = length(events)
	#				 		$unique, matrix with number of rows = length(unique(x))
	#				 		$coords, matrix with each row a point for plotting pav 
	#							   solution vs x for each unique prob estimate
	#
	events = data.df$Y
	trials = data.df$COUNT
	x = data.df$X
	x = rep(x, trials)
	events = rep(events, trials)
	trials = rep(1, length(x))
	k = length(events)
	if(length(x) == 0.) x = 1.:k else
		{
		events = events[order(x)]
		trials = trials[order(x)]
		x = sort(x)
		xuniq = unique(x)
		k = length(xuniq)
		evtmp = rep(0., k)
		tritmp = rep(0., k)
		for(i in 1.:k) {
		evtmp[i] = sum(events[x == xuniq[i]])
		tritmp[i] = sum(trials[x == xuniq[i]])
		}
	}
	events = evtmp
	trials = tritmp
	p = matrix(0., k, 1.)
	pp = matrix(0., k, k)
	for(i in 1.:k) {
		sum1 = 0.
		sum2 = 0.
		for(j in i:k) {
			sum1 = sum1 + events[j]
			sum2 = sum2 + trials[j]
			pp[i, j] = sum1/sum2
		}
		temp = as.matrix(pp[(1.:i), (i:k)])
		p[i] = ifelse(i > 1., max(apply(temp, 1., min)), min(temp))
	}
	puniq = unique(p)
	kk = length(puniq)
	xp = rep(0., kk)
	for(i in 1.:kk) {
		xp[i] = min(xuniq[p == puniq[i]])
	}

	if(ln) {xp=exp(xp); xplt = c(xp[1.], rep(xp[-1.], rep(2., length(xp) - 1.)), max(exp(x)));} else
	xplt = c(xp[1.], rep(xp[-1.], rep(2., length(xp) - 1.)), max(x))
	pplt = c(rep(puniq, rep(2., length(puniq))))
	if(plotit) {
		if(!lineit)
			plot(xplt, pplt, type = "n", xlab = labx, ylab = laby, main
				 = titl)
		lines(xplt, pplt, lwd = 2.)
	}
	xx = list(full = cbind(xuniq, events/trials, p), unique = cbind(xp, puniq),
		coords = cbind(xplt, pplt))
	return(xx)
}

blrb1=function()
{
x=
"
   This program executes the 3podm sensitivity test procedure as described in:
1. Wu, C. F. J, Tian, Y., Three-phase optimal design of sensitivity 
   experiments Journal of Statistical Planning and Inference 149 (2014), 1-15 
2. Wang, D. P., Tian, Y., Wu, C. F. Jeff, A Skewed Version of the Robbins-Munro-
   Joseph Procedure for Binary Response, Statistica Sinica (2015), 1679-1689
3. Wang, D. P., Tian, Y., Wu, C. F. Jeff, Comprehensive Comparisons of Major 
   Design Procedures for Sensitivity Testing, Journal of Quality 
   Technology, 52:2 (2020), 155-167
 
   Questions about the R code or use of the procedure may be directed to:
   Paul Roediger <proediger@comcast.net>, and/or
   Douglas Ray, US Army ARDEC, Picatinny Arsenal <douglas.m.ray.civ@mail.mil>

"
cat(x)
return()
}

blrb2=function()
{
x=
"
   This program executes the Neyer sensitivity test procedure as described in:
1. Neyer, Barry T., A D-Optimality-Based Sensitivity Test, Technometrics, 36, 
   61-70 (1994) 
2. Ray, D. M., Roediger, P. A., and Neyer, B. T., Commentary: Three-phase 
   optimal design of sensitivity experiments, Journal of Statistical Planning
   and Inference, 149, 20-25 (2014)
3. https://urldefense.com/v3/__http://neyersoftware.com/SensitivityTest/SensitivityTestFlyer.htm__;!!NFcUtLLUcw!FBP1WwABf1lGZZP1n8MQm-2OHcxaQ7ZNxnqIdlSHhU-4c9tjFXos_5b48OyzrE2ndd9e$ 

   Questions about the R code or use of the procedure may be directed to: 
   Paul Roediger, UTRS, Inc. <proediger@comcast.net>, and/or
   Douglas Ray, US Army ARDEC, Picatinny Arsenal <douglas.m.ray.civ@mail.mil>

"
cat(x)
return()
}

blrb3=function()
{
x=
"
   This program executes the Bruceton sensitivity test procedure* as described in:
1. Dixon, W. J., Mood, A. M., A method for obtaining and analyzing sensitivity
   data, Journal of the American Statistical Association, 43, pp. 109-126, (1948)
2. Dixon, W. J., Massey, F. J., Introduction to Statistical Analysis, McGraw-Hill, 2nd 
   Edition, Chapter 6, (1957)
3. Einbinder, S.K., Reliability Models and Estimation of Stress-Strength, 
   Dissertation, Polytechnic Institute of Brooklyn (1973)
4. Wetherill, G.B., Sequential Estimation of Quantal Response Curves, 
   Journal of the Royal Statististical Society, B, Vol. 25, pp. 1-48, (1963)

 * Developed at the Explosives Research Laboratory in Bruceton, PA (1941-1945)
   https://urldefense.com/v3/__http://www.dtic.mil/dtic/tr/fulltext/u2/116878.pdf__;!!NFcUtLLUcw!FBP1WwABf1lGZZP1n8MQm-2OHcxaQ7ZNxnqIdlSHhU-4c9tjFXos_5b48OyzrCPaNXtI$ 

   Questions about the R code or use of the procedure may be directed to:
   Paul Roediger, UTRS, Inc. <proediger@comcast.net>, and/or
   Douglas Ray, US Army ARDEC, Picatinny Arsenal <douglas.m.ray.civ@mail.mil>

"
cat(x)
return()
}

blrb4=function()
{
x=
"
   This program executes the Langlie sensitivity test procedure as described in:
1. Langlie, H. J., A Reliability Test Method For “One-Shot” Items, Publication
   No. U-1792, Aeronutronic Division of Ford Motor Company (1962)
2. Einbinder, S.K., Reliability Models and Estimation of Stress-Strength, 
   Dissertation, Polytechnic Institute of Brooklyn (1973)
3. Einbinder, S. K., One Shot Sensitivity Test for Extreme Percentage Points,
   ARO Report 74-1, Proceedings of the Nineteenth Conference on the Design of 
   Experiments in Army Research, Development and Testing, pp. 369-386, (1974)
   https://urldefense.com/v3/__http://www.dtic.mil/dtic/tr/fulltext/u2/a002564.pdf__;!!NFcUtLLUcw!FBP1WwABf1lGZZP1n8MQm-2OHcxaQ7ZNxnqIdlSHhU-4c9tjFXos_5b48OyzrHX902k_$ 
4. Wetherill, G.B., Sequential Estimation of Quantal Response Curves, 
   Journal of the Royal Statististical Society, B, Vol. 25, pp. 1-48, (1963)

   Questions about the R code or use of the procedure may be directed to: 
   Paul Roediger, UTRS, Inc. <proediger@comcast.net>, and/or
   Douglas Ray, US Army ARDEC, Picatinny Arsenal <douglas.m.ray.civ@mail.mil>

"
cat(x)
return()
}

blrb5=function()
{
x=
"

 This function requires two inputs, conf & J. Choose J from the following ...

          ------------------------------------------  ----------------------- 
         |   To Plot Confidence Interval(s) about:  ||   Via the Method(s)   |
         |  Probability (p)  |  Quantile (q) |  p&q ||  FM   |  LR   |  GLM  |
  -------|-------------------|---------------|------||-------|-------|-------|
 |       |         1         |        2      |   3  ||   X   |       |       |
 |       |-------------------|---------------|------||-------|-------|-------|
 |       |                   |               |   4  ||       |   X   |       |
 |       |-------------------|---------------|------||-------|-------|-------|
 | Enter |         5         |        6      |   7  ||       |       |   X   |
 | this  |-------------------|---------------|------||-------|-------|-------|
 | value |         8         |        9      |      ||   X   |   X   |       |
 |  for  |-------------------|---------------| -----||-------|-------|-------|
 |   J   |        10         |       11      |      ||   X   |       |   X   |
 |       |-------------------|---------------| -----||-------|-------|-------|
 |       |        12         |       13      |      ||       |   X   |   X   |
 |       |-------------------|---------------| -----||-------|-------|-------|
 |       |        14         |       15      |      ||   X   |   X   |   X   |
  ------- ------------------------------------------  ----------------------- 

"
cat(x)
return()
}

blrb6=function()
{
x=
"
  Three entries (separated by blanks) are required, namely -

     (1) nRev: the number of reversals needed to exit Phase I, and
     (2) two i values (chosen from the following table)
  
          i   Down(X=1,O=0)      Up(X=1,O=0)	      p      
        ---- --------------- --------------------- ----------  
       |  1 |             X |                  O  | .500000  |
       |  3 |            XX |             {O, XO} | .707107  |
       |  5 |           XXX |        {O, XO, XXO} | .793701  |
       |  7 |          XXXX |  {O, XO, XXO, XXXO} | .840896  |
       |  : |           :   |         :           |     :    |
        ---- --------------- --------------------- ---------  
       |  0 |            -  |                   - |     -    |
       |  2 |     {XX, XOX} |            {O, XOO} | .596968  |
       |  4 |   {XXX, XXOX} |       {O, XO, XXOO} | .733614  |
       |  6 | {XXXX, XXXOX} | {O, XO, XXO, XXXOO} | .804119  |
       |  : |       :       |       :             |     :    |
        ---- --------------- --------------------- ----------
               Up(X=0,O=1)       Down(X=0,O=1)	     1-p         

"
cat(x)
return()
}

# Two functions to solve equation 3.8 in skewed RMJ paper

f38=function(x,l)
{
a=(l-1)*pnorm(-x)+1;
b=(l-1)*dnorm(-x);
h=a*x-b;
return(h);
}

f3point8=function(l)
{
if(l <= 0 | l == 1) return(0);
x=0; del=0.2; h=1; 
v=f38(x,l);
# l > 1
if(v < 0)
{
while(h > 0)
{
ll=x;
x=x+del;
h=f38(x,l)*v;
}
ul=x;
}

# l < 1
if(v > 0)
{
while(h > 0)
{
ul=x;
x=x-del;
h=f38(x,l)*v;
}
ll=x;
}

eps=.000001;
w=10;
m=(ll+ul)/2;
while(abs(w) > eps)
{
w=f38(m,l);

if(w < 0) ll=m else ul=m;

m=(ll+ul)/2;
}
m=(ll+ul)/2;
return(m);
}

fgs=function(mlo,mhi,sg)
{
fg0=log((mhi+3*mlo)^3/(16*(3*mhi+mlo)))/2; 
fg1=log((3*mhi+mlo)^3/(16*(mhi+3*mlo)))/2;
fsg=(fg1-fg0)/7;
u=c(fg0,fg1,fsg);
return(u)
}

ifg=function(fg0,fg1)
{
m1=4*exp((fg0+3*fg1)/4);
m0=4*exp((3*fg0+fg1)/4);
mhi=(3*m1-m0)/8;
mlo=(3*m0-m1)/8;
v=c(mlo,mhi);
return(v)
}

addneyr=function(dtt,ylm,sim=F)
{
tf=F;
id=dtt$ID;
nid=length(id); 
x=dtt$X;
s1=c("B0","B1","B2","B3","B4","II1","II2","III1","III2");
s2=c(rep("I",5),rep("II",2),rep("III",2)); ns=length(s1);
for(i in 1:ns) id=gsub(s1[i],s2[i],id);
u=id[1]; vee=numeric(0);
# vee = index of the first test that isn't I, i.e., vee=numeric(0) when you're still in I
if(nid > 1){for(i in 2:nid)	if(id[i]!=u) {vee=c(vee,i); u=id[i];}}
nv=length(vee);
ul=c(vee-1,nid); ll=c(1,vee); 
ml=(ll+ul)/2; lab=unique(id);
iv=2; if(nv<=1)iv=1;

text(ml,rep(ylm[2],nv+1),lab,cex=.9);

if(nv == 0)
{
j=m.update(dtt); 
M0=j$M0; 
m1=j$m1; 
w0=which(x==M0); 
w1=which(x==m1);
vc=c(M0,m1,w0,w1);

if(!any(is.na(vc)) & M0 > m1) 
	{
	lines(c(w0[[1]],nid+1),c(M0,M0),col=3,lty=4); lines(c(w1[[1]],nid+1),c(m1,m1),col=4,lty=4);
	tf=T;
	}
}

if(nv > 0)
	{
	lt=rep(5,nv); abline(v=vee-.5,lty=lt);

	j=m.update(dtt[1:(vee[1]-1),]); 
	M0=j$M0; 
	m1=j$m1; 
	w0=which(x==M0); 
	w1=which(x==m1);
	lines(c(w0[[1]],vee[1]-.5),c(M0,M0),col=3,lty=4); lines(c(w1[[1]],vee[1]-.5),c(m1,m1),col=4,lty=4);
	}
	kp=0;
	if(sim) 
	{
	k=1;
	if(nv >= 1)k=vee[iv]-1;
	for(j in k:nid) {	jj=m.update(dtt[1:j,]); M0=jj$M0; m1=jj$m1; uv=c(M0,m1); if(any(is.na(uv))) break; if(M0 > m1) kp=j; if(kp > 0) break;	}
	}
return(c(tf,kp))
}

add3pod=function(dtt,ylm,sim=F)
{
tf=F;
id=dtt$ID;
nid=length(id); 
x=dtt$X;
s1=c("r","I1\\(i\\)","I1\\(ii\\)","I1\\(iii\\)","I1\\(iv\\)","I2\\(ib\\)","I2\\(ic\\)","I2\\(id\\)","II1","II2","III1","III2");
s2=c("",rep("I1",4),rep("I2",3),rep("II",2),rep("III",2)); ns=length(s1);
for(i in 1:ns) id=gsub(s1[i],s2[i],id);
u=id[1]; vee=numeric(0);
# vee = index of the first test that isn't I1, i.e., vee=numeric(0) when you're still in I1
if(nid > 1){for(i in 2:nid)	if(id[i]!=u) {vee=c(vee,i); u=id[i];}}
nv=length(vee);
ul=c(vee-1,nid); ll=c(1,vee); 
ml=(ll+ul)/2; lab=unique(id);
iv=2; if(nv<=1)iv=1;

text(ml,rep(ylm[2],nv+1),lab,cex=.9);
lt=c(2,2); if(nv > 2) lt=c(lt,rep(4,nv-2)); lt=lt+1;

if(nv > 0)
{
	abline(v=vee-.5,lty=lt);
	j=m.update(dtt[1:(vee[iv]-1),]);
	M0=j$M0; 
	m1=j$m1; 
	w0=which(x==M0); 
	w1=which(x==m1);
		
	if((nv > 1 | length(vee) != 0) & M0 > m1)
	{
	lines(c(w0[[1]],vee[iv]-.5),c(M0,M0),col=3,lty=4); lines(c(w1[[1]],vee[iv]-.5),c(m1,m1),col=4,lty=4);
	} else
	{
		j=m.update(dtt); 
		M0=j$M0; 
		m1=j$m1; 
		w0=which(x==M0); 
		w1=which(x==m1);
		if(M0 > m1)
		{
		lines(c(w0[[1]],nid+1),c(M0,M0),col=3,lty=4); lines(c(w1[[1]],nid+1),c(m1,m1),col=4,lty=4);
		}
	}
}
	kp=0;
	if(sim)
	{
	k=1;
	if(nv >= 1)k=vee[iv]-1;
	for(j in k:nid) {	jj=m.update(dtt[1:j,]); M0=jj$M0; m1=jj$m1; uv=c(M0,m1); if(any(is.na(uv))) break; if(M0 > m1) kp=j; if(kp > 0) break;	}
	}
return(c(tf,kp))
}

addBorL=function(dtt,ylm,ud)
{
id=dtt$ID; lid=length(id);
w1=which(is.element(id,c("IB","IL","D","U",""," ")));
l1=length(w1);
if(l1 > 0) 
{
en1=max(w1); a=is.element (id,c(" ","D","U")); b=any(a);
if(b & ud) {mx=max(which(a)); text(1:mx,rep(ylm[2],mx),id[1:mx],cex=.6);} else
text(mean(range(w1)),ylm[2],"I",cex=.9);

	if(l1 > 1)
	{
	dtw=dtt[1:en1,];
	xx=dtw$X; yy=dtw$Y;
	j=m.update(dtw); 
	M0=j$M0; 
	m1=j$m1; 
	del=m1-M0;
		if(!is.na(del))
		{
		w0=which(xx==M0 & yy==0); 
		w1=which(xx==m1 & yy==1);
		if(del < 0)
			{
			lines(c(w0[[1]],en1+.5),c(M0,M0),col=3,lty=4); 
			lines(c(w1[[1]],en1+.5),c(m1,m1),col=4,lty=4);
			}
		}
	}
}

w2=which(is.element(id,c("II","II1","II2")));
l2=length(w2); 
if(l2 > 0) {en2=max(w2); abline(v=en1+.5,lty=5);
text(mean(range(w2)),ylm[2],"II",cex=.9);
}

w3=which(is.element(id,c("III","III1","III2","III3")));
l3=length(w3);
if(l3 > 0) {abline(v=en2+.5,lty=5); en3=max(w3);
text(mean(range(w3)),ylm[2],"III",cex=.9);}
return()
}

ptest=function(dat,plt,notitle=F)
{
 if(!is.element(plt,1:8))
 {
 u=paste("plt must be 1, 2, 3, 4, 5, 6, 7 or 8.\nTry again.\n\n",sep="");
 cat(u);
 return()
 }
 if(plt < 4)
 {
  if(is.null(dat$tmu))
  {
  if(plt == 1) {{if(!notitle)pdat1(dat)     else pdat1(dat,notitle=T)};   return();}
  if(plt == 2) {{if(!notitle)v=pdat2(dat)   else v=pdat2(dat,notitle=T)};   return(v);}
  if(plt == 3) {{if(!notitle)v=pdat3(dat)   else v=pdat3(dat,notitle=T)};   return(v);}
  } else
  {
  if(plt == 1) {{if(!notitle)pSdat1(dat)    else pSdat1(dat,notitle=T)};  return();}
  if(plt == 2) {{if(!notitle)v=pSdat2(dat)  else v=pSdat2(dat,notitle=T)};  return(v);}
  if(plt == 3) {{if(!notitle)v=pSdat3(dat)  else v=pSdat3(dat,notitle=T)};  return(v);}
  }
 } else
  {
  if(plt == 4) {picdat(dat); return();}
  if(plt == 5) {{if(!notitle)v=jlrcb(dat)   else v=jlrcb(dat,notitle=T)};   return(v);}
  if(plt == 6) {{if(!notitle)v=lrcb(dat)    else v=lrcb(dat,notitle=T)};    return(v);}
  if(plt == 7) {{if(!notitle)v=cbs(dat,plt) else v=cbs(dat,plt,notitle=T)}; return(v);}
  if(plt == 8) {{if(!notitle)v=cbs(dat,plt) else v=cbs(dat,plt,notitle=T)}; return(v);}
  }
}

# Reset some par() values that may have been changed while graphing

reset=function()
{
par(mar=c(5,4,4,2)+.1, oma=c(0,0,0,0), mgp=c(3,1,0));
return()
}

# two handy alpha vectors (of length 15 and 49). useful to get confidence
# interval table outputs from the lims function (with P=al15, or P=al49).

al15=c(1,10,100,1000,10000,100000,250000)/1000000; al15=c(al15,.5,sort(1-al15));
al49=10^(6:2); al49=c(1/al49,1-1/al49); al49=c(al49,seq(25,975,by=25)/1000); al49=sort(al49);

lims=function(ctyp,dat,conf,P=numeric(0),Q=numeric(0))
{
if(!is.element(ctyp,1:3))return()
np=length(P); nq=length(Q); npq=np+nq;
if(npq == 0 | any(P < 0 | P > 1)) return()
switch(ctyp,
{
nam="FM"; z=fm.lims(dat,conf,P,Q);
},
{
nam="LR"; z=lrq.lims(dat,conf,P,Q); z=z$lrmat;
},
{
nam="GLM"; z=glm.lims(dat,conf,P,Q);
}
);
z=round(z,6);
return(z);
}

cpq=function(P,Q,mu,sig,gt)
{
k=10;
n=1000000.0;
p0=1/(k*n); 
p1=1-p0; 
q0=mu+qnorm(p0)*sig;
q1=mu+qnorm(p1)*sig;
val=c(p0,p1,q0,q1);
ok=T;
  # The 1st line was operative in the simulation version 
  # Let's use the 2nd line that's used in the console version
if(is.null(gt)){if(any(P < p0) | any(P > p1)) ok=F;} else
  {if(any(P < p0) | any(P > p1) | any(Q < q0) | any(Q > q1)) ok=F;}
if(!ok) 
{
v=round(val,10);
l7=paste("\nAll P's must be in [",v[1],", ",v[2],"]\n",sep="");
v=round(v,4);
l8=paste("All Q's must be in [",v[3],", ",v[4],"]\n",sep="");
cat(l7);
cat(l8);
cat("Try again\n\n");
}
return(ok)
}

fm.lims=function(dat,conf,P=numeric(0),Q=numeric(0))
	{
	# Calculates qlo, qhi, plo & phi as in ml2002
	np=length(P); nq=length(Q); npq=np+nq;
	if(npq == 0 | any(P < 0 | P > 1)) return(); 
	x=rep(dat$X,dat$COUNT);	y=rep(dat$Y,dat$COUNT); gt=dat$tmu;
	r0=x[y==0]; r1=x[y==1];
	M0=max(r0); m1=min(r1); del=m1-M0; one23=2+sign(del);
	# overlap: one23=1 (interval); one23=2 (point); one23=3 (none);
	if(one23 > 1)
	{
	cat("\nFM confidence intervals exist only if there's interval overlap\n\n");
	return()
	}
	xglm=glm(y ~ x, family = binomial(link = probit));
	ab=as.vector(xglm$coef);
	muhat=-ab[1]/ab[2];
	sighat=1/ab[2];
	chk=cpq(P,Q,muhat,sighat,gt);
	if(!chk) return();
	al=c(P,pnorm((Q-muhat)/sighat))
	w0=which(al == 0)
	w1=which(al == 1)
	if(length(w0) > 0) al[w0]=1e-6
	if(length(w1) > 0) al[w1]=1-1e-6
	# CI's for Normal
	qp=qnorm(al);
	vcov1=yinfomat(dat,muhat,sighat,solv=T)$vcov1*sighat^2;
	varq=vcov1[1,1]+qp*(qp*vcov1[2,2]+2*vcov1[1,2]);
	zscore=qnorm((1+conf)/2)
	# With this zscore, get GLM limits ql & qu (essentially same as mdose.p)
	# However, with this zscore, you don't get a match with GLM's pl & pu
	# zscore=qt((1+conf)/2,xglm$df.residual)
	zdel=zscore*sqrt(varq);
		q0=muhat+qnorm(al)*sighat;
	if(nq > 0) {iq=c(nq:1) - 1; q0[npq-iq]=Q;} 
		qlo=muhat+qp*sighat-zdel;
		qhi=muhat+qp*sighat+zdel;
	dpda=1/sqrt(2*pi)*exp(-0.5*qp^2)/sighat;
	zdel=dpda*zdel;
		plo=al-zdel; plo[which(plo<0)]=0;
		phi=al+zdel; phi[which(phi>1)]=1;
	rd=6;
	qlo=round(qlo,rd); q0=round(q0,rd); qhi=round(qhi,rd);	
	plo=round(plo,rd); phi=round(phi,rd);
	return(matrix(c(qlo,q0,qhi,plo,al,phi),ncol=6));
	}

glm.lims=function(dat,conf,P=numeric(0),Q=numeric(0))
	{
	# Calculates qlo & qhi (GLM) and plo & phi (mdose.p)
	np=length(P); nq=length(Q); npq=np+nq;
	if(npq == 0 | any(P < 0 | P > 1)) return();
	x=rep(dat$X,dat$COUNT);	y=rep(dat$Y,dat$COUNT); gt=dat$tmu;
	r0=x[y==0]; r1=x[y==1];
	M0=max(r0); m1=min(r1); del=m1-M0; one23=2+sign(del);
	
	# overlap: one23=1 (interval); one23=2 (point); one23=3 (none);
		
	xglm=glm(y ~ x, family = binomial(link = probit));
	ab=as.vector(xglm$coef);
	muhat=-ab[1]/ab[2];
	sighat=1/ab[2];

	chk=cpq(P,Q,muhat,sighat,gt);
	if(!chk) return();
	al=c(P,pnorm((Q-muhat)/sighat));
	q0=muhat+qnorm(al)*sighat;
	if(nq > 0) {iq=c(nq:1) - 1; q0[npq-iq]=Q;} 
	yy=predict(xglm, list(x = q0), se.fit = T);
	df=sum(dat$COUNT)-2;
	conf=(1+conf)/2; 
        k=qt(conf,df);
	yu=yy$fit+k*yy$se.fit;
	yl=yy$fit-k*yy$se.fit;
	rd=6
	plo=round(pnorm(yl),rd);phi=round(pnorm(yu),rd);
 	p0=pnorm(yy$fit);
	ug=mdose.p(xglm,al);
	ssee=ug$se;
	qlo=round(q0-qt(conf,xglm$df.residual)*ssee,rd);
	qhi=round(q0+qt(conf,xglm$df.residual)*ssee,rd);
	q0=round(q0,rd); 
	return(matrix(c(qlo,q0,qhi,plo,al,phi),ncol=6));
	}

lrq.lims=function(dat,conf1,P=numeric(0),Q=numeric(0))
{
# Check if there's a ZMR, and calculate muhat, sighat, and llik
# If No ZMR, muhat=mid point, sighat=.0001
	np=length(P); nq=length(Q); npq=np+nq;
	chk=mixed(dat); 
	dif=chk$dif; muhat=chk$summ/2; sighat=0.0001; min1=chk$min1; max0=chk$max0; con=chk$con;
	if(npq == 0 | any(P < 0 | P > 1)) return() else al=c(P,pnorm((Q-muhat)/sighat));
	conf2=pchisq(qchisq(conf1,1),2);
	if(dif >= 0) conf2=(conf2+3)/4;

	if(dif < 0)
		{
		rx=rep(dat$X,dat$COUNT); ry=rep(dat$Y,dat$COUNT); 
		xglm=glm(ry~rx,family=binomial(link=probit));	
		ab=as.vector(xglm$coef);
		muhat=-ab[1]/ab[2];
		sighat=1/ab[2];
		}
		
		uu=llik(dat,muhat,sighat);
	
	levs0=uu+log(1-conf2); levs1=levs2=uu-qnorm((1-conf1)/2)^2/2;

options(warn=-1); # SUPPRESSES OUT OF BOUNDS WARNINGS (AND OTHERS)

# Contour (cx,cy): llik=levs0 needed for Graphs 1, 2 & 3
degs=180;
if(dif > 0)degs=360;
st=ct=cx=cy=tr=(0:360)*pi/degs;

x0=as.vector(muhat); 
y0=ru=as.vector(sighat);
eps=0.000001; eps4=.0001;
ibot=2; itop=361;
if(dif > 0){cy[1]=cy[361]=0;cx[1]=min1;cx[361]=max0;ibot=2;itop=360;ru=1;}
	
for(i in 0:361) {st[i]=sin(tr[i]); ct[i]=cos(tr[i]);}

for(i in 1:181)
{
xl=x0; yl=y0;
xu=x0+ru*ct[i]; yu=y0+ru*st[i];
while(llik(dat,xu,yu)>levs0){xl=xu; yl=yu; xu=xu+ct[i];yu=yu+st[i];}
x=(xl+xu)/2; y=(yl+yu)/2;

lval=llik(dat,x,y);
zz=abs(lval-levs0)

while(zz>eps)
{
if(lval>levs0){xl=x;yl=y;} else {xu=x;yu=y;}
x=(xl+xu)/2; y=(yl+yu)/2
lval=llik(dat,x,y);
zz=abs(lval-levs0)
}
cx[i]=(xl+xu)/2;cy[i]=(yl+yu)/2;
}

for(i in 182:360)
{
dm=ct[i]*y0/st[i]; i2=25; i1=i2-1;
xu=x0+dm*(1-i1/i2); yu=y0*i1/i2;
while(llik(dat,xu,yu)>levs0){i2=i2+1; xl=xu; yl=yu; xu=x0+dm*(1-i1/i2); yu=y0*i1/i2;}

x=(xl+xu)/2; y=(yl+yu)/2;

lval=llik(dat,x,y);
zz=abs(lval-levs0)

while(zz>eps)
{
if(lval>levs0){xl=x;yl=y;} else {xu=x;yu=y;}
x=(xl+xu)/2; y=(yl+yu)/2
lval=llik(dat,x,y);
zz=abs(lval-levs0)
}
cx[i]=(xl+xu)/2;cy[i]=(yl+yu)/2;
}
cx=c(cx[1:181],cx[360:182]);
cy=c(cy[1:181],cy[360:182]);
cx[361]=cx[1]; cy[361]=cy[1];

lrmat=matrix(rep(0,6*npq),ncol=6);lrmat[,5]=c(P,pnorm((Q-muhat)/sighat))

lrmat[,2]=muhat+qnorm(lrmat[,5])*sighat;	
for(i in 1:npq)
{
	lrmat[i,1]=min(cx+qnorm(lrmat[i,5])*cy,na.rm=T); 
	lrmat[i,3]=max(cx+qnorm(lrmat[i,5])*cy,na.rm=T);
	lrmat[i,4]=min(pnorm((lrmat[i,2]-cx)/cy),na.rm=T); 
	lrmat[i,6]=max(pnorm((lrmat[i,2]-cx)/cy),na.rm=T);
}
ret=list(cx,cy,lrmat,muhat,sighat,dif,con);
names(ret)=c("cx","cy","lrmat","muhat","sighat","dif","con");

return(ret);
}

qrda = function(dat, conf=.9, J=2, ln = F, labx = "", laby = "Probability of Response", zee = 0)
{
	c1sided=(1+conf)/2; 
	ldot=3.;
	x = xsav=rep(dat$X, dat$COUNT);
	y = ysav=rep(dat$Y, dat$COUNT);
	xglm = glm(y ~ x, family = binomial(link = probit))
	ab = as.vector(xglm$coef);
	a=ab[1]
	b=ab[2]
	mu= -a/b
	sig=1/b
	if(ln) k=2.5 else k=3.5; 
	pm=c(-1,1); pee=pnorm(pm*k);
	if(!ln) a1=pretty(mu+k*sig*pm) else a1=pretty(qlnorm(pee,meanlog=mu,sdlog=sig))
	if(!ln) a2 = range(c(a1,range(x))) else a2=range(c(a1,range(dat$RX)));
	if(ln) a2[2]=min(a2[2],1.5*max(exp(x)));
	xs = seq(a2[1], a2[2], length = 100.);
	if(ln) {if(xs[1] == 0) xs[1]=xs[2]/100; xs=log(xs);}
	yy = predict(xglm, list(x = xs), se.fit = T);
	yn = pnorm(yy$fit);
	ys=1:99/100; ys=c(.001,.005,ys,.995,.999);

	if(ln) xs = exp(xs)
	plot(xs, yn, ylim = c(0,1), type = "n", las = 1., cex=.6, xlab = "", ylab = "",xaxt="n",yaxt="n")
	axis(1,at=pretty(xs),labels=T,tck=-.01,cex.axis=.9,mgp=c(3,.4,0));
	axis(2,at=pretty(yn),labels=T,tck=-.01,cex.axis=.9,mgp=c(3,.4,0),las=1);
	mtext(expression(paste("Probability ( ",italic(p),")",sep="")),side=2,line=2.8,cex=1);
	if(labx == "")x123=expression(paste("Quantile (",italic(q),")",sep="")) else
	x123=substitute(paste("Quantile (",italic(q),", in units ",labx,")",sep=""),list(labx=labx));
	mtext(x123,side=1,line=2,cex=1);
	rd=4; uvw=round(100*conf,rd); ax="p";
	abline(h = 0.1 * c(0.:10.), lty = ldot)
	
	if(!ln) {abline(v = pretty(a2), lty = ldot); dpts=dnorm(xs,mean=mu,sd=sig);} else 
	{abline(v=pretty(range(xs)),lty=ldot); dpts=dlnorm(xs,meanlog=mu,sdlog=sig);}
	em=max(dpts);
	lines(xs,dpts/em,type="l",col=8,lwd=2);

	pavdf(dat, ln, plotit = T, lineit = T)
	nxv=length(xsav);
	if(!ln) {for(i in 1:nxv) points(xsav[i],ysav[i]+(ysav[i]-.5)/25,pch=4,lwd=1.5,cex=.5);} else
	{for(i in 1:nxv) points(exp(xsav[i]),ysav[i]+(ysav[i]-.5)/25,pch=4,lwd=1.5,cex=.5);}
	
	cl=list(1,1,1,2,3,3,3,c(1,2),c(1,2),c(1,3),c(1,3),c(2,3),c(2,3),c(1,2,3),c(1,2,3));
	qpl=list(1,2,c(1,2),c(1,2),1,2,c(1,2),1,2,1,2,1,2,1,2);

	colo=c(4,1,2); lin=c(1,1,1);
	si=c(2,1,3,3,2,1,3,2,1,2,1,2,1,2,1);

	x35=substitute(paste(uvw,"%  Confidence  Interval  about     ",italic(p),sep=""),list(uvw=uvw));
	x46=substitute(paste(uvw,"%  Confidence  Interval  about   ",italic(q),sep=""),list(uvw=uvw));

	ct=cl[[J]]; qpt=qpl[[J]];
	n1=length(ct); n2=length(qpt);
	LJ=vector("list",n1*n2);
	if(si[J] == 1 | si[J] == 3) mtext(x46,side=1,line=3.3,cex=1);
	if(si[J] == 2 | si[J] == 3) mtext(x35,side=2,line=1.5,cex=1);

	ij=0;
	if(ln) xs=log(xs);

for(i in 1:n1)
{
	for(j in 1:n2)
	{
	ij=ij+1; 
	cti=ct[i]; qpi=qpt[j];
	if(qpi == 1) yyy=lims(cti,dat,conf,Q=xs) else yyy=lims(cti,dat,conf,P=ys);
	if(ln) { yyy[,1:3] = exp(yyy[,1:3]); }
	if(ij==1) lines(yyy[,2],yyy[,5],lwd=2);
	LJ[[ij]]=yyy;
	if(qpi == 1) { lines(yyy[,2],yyy[,4],col=colo[cti]); lines(yyy[,2],yyy[,6],col=colo[cti]); } else
		 	 { lines(yyy[,1],yyy[,5],col=colo[cti],lty=4); lines(yyy[,3],yyy[,5],col=colo[cti],lty=4); }
	}
}
	if(ln) xs=exp(xs);
	dx=xs[1]+diff(range(xs))/25;
	leg=c("FM","LR","GLM"); pq=c("p","q");
	legend(dx,.93,legend=leg[ct],lty=lin[ct],col=colo[ct],lwd=3,cex=.6,bg="white");
	nlj=paste(leg[ct],pq[qpt],sep="");
	names(LJ)=nlj; 
	xx=list(xglm=xglm, a=a, b=b, mu=mu, sig=sig, J=J, LJ=LJ); 
	reset()
	return(xx)
}

prtrans=function(i)
{
# i is a vector of length 2
dud=lev=numeric(0);
for(j in 1:length(i))
{
v=i[j];
if(v > 0)
{
nSeq=1+(v-v%%2)/2;
nAdd=1-v%%2;
L=udli(v)
nL=bintodec(L);
if(j==1) GE5=T else GE5=F
if(GE5) {au="U = {"; ad="D = {";} else {au="D = {"; ad="U = {";}
if(GE5) d9=unlist(lapply(L,"fofL")) else d9=unlist(lapply(lapply(L,"iofL"),"fofL"))
du=paste(au,paste(d9[1:nSeq],collapse=", "),"}",sep="");
dd=paste(ad,paste(d9[(nSeq+1):(nSeq+nAdd+1)],collapse=", "),"}",sep="");
zv=zpfun(v);
if(!GE5) zv=1-zv;
lev=c(lev,zv);
dud=c(dud,paste(du,", ",dd,", Lev = ",round(zv,6),sep=""));
}
}
return(list(dud=dud,lev=lev))
}

fofL=function(L) return(paste(L,collapse=""))
iofL=function(L) return(1-L)

bintodec = function(y)
{
  # y is now a LIST of vectors consisting of 0's and 1's
  # find the decimal number corresponding to binary sequence 'y'
  # L is not a list, L is a vector of integers
ny=length(y)
L=numeric(0)
for(i in 1:ny)
{
yi=y[[i]]  
  if (! (all(yi %in% c(0,1)))) stop("not a binary sequence")
  res = sum(yi*2^((length(yi):1) - 1))
L[[i]]=res
}
  return(L)
}

udli=function(i)
{
iadd=1-i%%2;
n=(i+iadd+1)/2;
L=vector("list",n+1)
for(j in 0:(n-1))L[[j+1]]=bintodec(c(rep(1,j),0))
L[[n+1]]=bintodec(rep(1,n))
if(iadd==1)
{
L[[n+2]]=L[[n+1]]
L[[n+1]]=c(L[[n]],1)
L[[n]]=c(L[[n]],0)
}
return(L)
}

pfun=function(pee,n)
{
# pfun(0) < 0, pfun(1) > 0
return(4*pee^n-2*pee^(n+1)-1)
}


zpfun=function(i)
{
em=numeric(0);
for(j in 1:length(i))
{
if(i[j]%%2 == 1) mm=.5^(2/(i[j]+1)) else
 {
 eps=.000001;
 ll=0
 ul=1
 w=10;
 m=1/2;
 while(abs(w) > eps)
   {
   w=pfun(m,1+i[j]/2);
   if(w < 0) ll=m else ul=m;
   m=(ll+ul)/2;
   }
   mm=m
 }
em=c(em,mm);
}
return(em);
}

xlead0=function(num,dig)
{
n=round(num,dig)
nc=as.character(n);
wh=which(abs(n) < 1);
nw=n[wh];
nwc=gsub("0.",".",nw,fixed=T)
nc[wh]=nwc;
return(nc)
}

# INDEX 97 through 108, XjlrcbS3.R (12 functions)

xyllik = function(rx,ry,m,s)
{
kx=rx-m;
ns=length(s);
ll=numeric(0);
for(i in 1:ns)
{
pms=(2*ry-1)*s;
ll=c(ll,sum(log(pnorm(kx/pms))));
}
return(ll);
}

stopQuietly = function(...) 
{
  blankMsg = sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
} 

# reset is used in this suite of functions; it's defined just once in gonogo.R

calcblim=function(bl,ul)
{
# Calculate limits on plot 1 based on just bounded (or combined) regions
mlim=slim=numeric(0);
num=length(bl);
if(length(bl[[1]][[1]]) != 1)
{
for(k in 1:num){
bk=bl[[k]]; mlim=range(c(mlim,bk[[1]])); slim=range(c(slim,bk[[2]]));
}
}
num=length(ul);
if(length(ul[[1]][[1]]) != 1){
for(k in 1:num){
bk=ul[[k]]; mlim=range(c(mlim,bk[[1]],bk[[3]])); slim=range(c(slim,bk[[2]],bk[[4]]));
}
}
a=list(mlim,slim);
names(a)=c("mlim","slim");
return(a)
}

mkb0=function(confv)
{
# adapted from llik1.R
ncl=length(confv);
b0=round(confv,3);
b0=paste(b0,collapse=",")
b0=gsub(",0.",",.",b0,fixed=T);
b0=gsub("0.",".",b0,fixed=T);
w1=w2="";
if(ncl > 1) {w1="("; w2=")";}
b0=substitute(paste(w1,b0,w2,sep=""));
return(b0)
}

unbd=function(rx,ry,levs,mh1,mh2,es,mlim)
{
		mh=(mh1+mh2)/2;
		if(length(mlim) == 2){L=mlim[1]; U=mlim[2];} else
		{
		pm=c(-1,1); I=c(mh1,mh2)+0.5*mh*pm; L=I[1]; U=I[2];
		}  
		if(es > 0) S0=ST1=ulik(rx,ry,levs,mh1,es) else S0=ST1=0;
		num=51;
		J1=seq(mh1,L,length=num); 
		for(i in 2:num)
		{
		S1=uliknext(rx,ry,levs,J1[i-1],S0,J1[i]);
		ST1=c(S1,ST1);
		S0=S1;
		}		
		if(es > 0) S0=ST2=ulik(rx,ry,levs,mh2,es) else S0=ST2=0;
		J2=seq(mh2,U,length=num); 
		for(i in 2:num)
		{
		S1=uliknext(rx,ry,levs,J2[i-1],S0,J2[i]);
		ST2=c(ST2,S1);
		S0=S1;
		}
		mt1=J1[num:1]; mt2=J2; 
		return(list(mt1,ST1,mt2,ST2));
}

uliknext=function(rx,ry,levs0,em1,es,em2)
{
if(es == 0) es=.1;
shi=es; slo=es/2; 
val1=rem1=xyllik(rx,ry,em2,slo);
val2=rem2=xyllik(rx,ry,em2,shi);

# val may at first decrease (good) but then increase (bad)
while(val1 > levs0) 
{
shi=slo; slo=slo/2; val1=xyllik(rx,ry,em2,slo);
if(val1 > rem1) 
	{
	cat(paste("Message from uliknext: conf1 is LARGER & TOO NEAR c1max.\n",sep=""))
	cat(paste("The specific problem is: for m = ",round(em2,4),", val1(s) > levs0 for all s.\n",sep=""));
	cat(paste("Increasing conf1 can produce a more clearly defined UNBOUNDED region.\n",sep=""));
	stopQuietly();	
	} else rem1=val1;
}

# val2 may at first increase (good) but then decrease (bad)
while(val2 < levs0) 
{
slo=shi; shi=2*shi; val2=xyllik(rx,ry,em2,shi);
if(val2 < rem2) 
	{
	cat(paste("Message from uliknext: conf1 is LARGER & TOO NEAR c1max.\n",sep=""))
	cat(paste("The specific problem is: for m = ",round(em2,4),", val2(s) < levs0 for all s.\n",sep=""));
	cat(paste("Increasing conf1 can produce a more clearly defined UNBOUNDED region.\n",sep=""));
	stopQuietly();
	} else rem2=val2;
}

eps=.0001; delt=1;
while(delt > eps)
{
s=(slo+shi)/2;
val=xyllik(rx,ry,em2,s);
if(val > levs0) shi=s else slo=s;
delt=abs(val-levs0);
}
s=(slo+shi)/2;
return(s);
}

ulik=function(rx,ry,levs0,em,es)
{
shi=es; slo=es/2; val1=val2=xyllik(rx,ry,em,slo);
while(val1 > levs0) {shi=slo; slo=slo/2; val1=xyllik(rx,ry,em,slo);}
while(val2 < levs0) {slo=shi; shi=2*shi; val2=xyllik(rx,ry,em,shi);}
eps=.00001; delt=1;
while(delt > eps)
{
s=(slo+shi)/2;
val=xyllik(rx,ry,em,s);
if(val > levs0) shi=s else slo=s;
delt=abs(val-levs0);
}
s=(slo+shi)/2;
return(s);
}

otherpoint=function(rx,ry,muhat,levs0,con)
{
k=1.5; slo=Inf; shi=-Inf;
s=1;
while(slo > shi)
{
m=muhat-qnorm(con)*s;
val=xyllik(rx,ry,m,s);
if(val > levs0) {slo=s; s=k*s;} else {shi=s; s=s/k;}
}
eps=.00001;
delt=1;
while(delt > eps)
{
s=(slo+shi)/2;
m=muhat-qnorm(con)*s;
val=xyllik(rx,ry,m,s);
if(val > levs0) slo=s else shi=s;
delt=abs(val-levs0);
}
s=(slo+shi)/2;
m=muhat-qnorm(con)*s;
return(c(m,s));
}

jlik=function(rx,ry,levs0,ms,op,one23)
{
# Contour (cx,cy): llik=levs0 needed for Graphs 1, 2 & 3

ndeg=361; ang=0; h=1; 
if(one23 == 2) {d=op-ms; d7=(op+ms)/2; ang=atan(d[1]/d[2])+pi/2;}
if(one23 == 3) h=2;
cx=cy=tr=2*(0:(ndeg-1))*pi/(h*(ndeg-1))-ang;

rl=0;
if(one23 == 1) 
{
ibot=1; itop=ndeg; 
x0=as.vector(ms[1]); y0=ru=as.vector(ms[2]);
}
if(one23 == 2) 
{
ibot=2; itop=ndeg-1;
x0=as.vector(d7[1]); y0=ru=as.vector(d7[2]);
cx[1]=cx[ndeg]=ms[1]; 
cy[1]=cy[ndeg]=ms[2];
}
if(one23 == 3) 
{
ibot=2; itop=ndeg-1;
x0=ms[1]; y0=0; ru=1;
cx[1]=op[1]; cx[ndeg]=op[2]; 
cy[1]=cy[ndeg]=0; 
}

eps=0.000001;
        
for(i in ibot:itop)
{
st=sin(tr[i]); 
ct=cos(tr[i]);

xl=x0+rl*ct;
yl=y0+rl*ct;

xu=x0+ru*ct; yu=y0+ru*st;
while(xyllik(rx,ry,xu,yu)>levs0){xu=xu+ct;yu=yu+st;}
x=(xl+xu)/2; y=(yl+yu)/2;
lval=xyllik(rx,ry,x,y);
zz=abs(lval-levs0)

while(zz>eps)
{
if(lval>levs0){xl=x;yl=y;} else {xu=x;yu=y;}
x=(xl+xu)/2; y=(yl+yu)/2
lval=xyllik(rx,ry,x,y);
zz=abs(lval-levs0)
}
cx[i]=(xl+xu)/2;cy[i]=(yl+yu)/2;
}
return(list(cx,cy))
}

jlrcb=function(dat,notitle=F)
{
# c2max = function of uu(muhat,sighat,rx,ry) and llc(con(rx,ry))
# AN IDENTITY: levs = uu+log(1-conf2) = 1-qchisq(conf1,1)/(2*uu);

dt=dat$d0; titl1=dat$title; test=dat$test; about=dat$about;
ttl0=dat$ttl0; ttl1=dat$ttl1; ttl2=dat$ttl2; tmu=dat$tmu;
tnam=c("3pod","Neyer");

xx="Enter conf's (separated by blanks): ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));
vconf1=sort(unique(xx));
vconf2=pchisq(qchisq(vconf1,1),2);
nc1=length(vconf1);
rx=dt$X; ry=dt$Y; nc=dt$COUNT;
rx=rep(rx,nc); ry=rep(ry,nc); 

r0=rx[ry==0]; r1=rx[ry==1]; 
mix=length(r0)*length(r1);
lux=length(unique(rx));
if(mix == 0 | lux == 1)	
{
cat(paste("Need to do more testing\n",sep="")); 
stopQuietly();
}

nt=sum(nc);
con=sum(ry)/length(ry);
llc=sum(log(con^ry*(1-con)^(1-ry)));
 
M0=max(r0); m1=min(r1); del=m1-M0; one23=2+sign(del);

bl=ul=list(1)		# placeholder for bounded & unbounded lists
numb=numu=0;		# eventual number of bounded & unbounded plots
mlim=0;			# default value to pass into unbd (if all are unbounded)
icbl=T;
for(i in 1:nc1)
{
conf2=pchisq(qchisq(vconf1[i],1),2);

switch(one23,
{	# OVERLAP (Interval) (use log lik)

	xglm=glm(ry~rx,family=binomial(link=probit));	
	ab=as.vector(xglm$coef);
	muhat=-ab[1]/ab[2]; sighat=1/ab[2];
	uu=xyllik(rx,ry,muhat,sighat);
	levs=uu+log(1-conf2); 
	c2max=pchisq(2*(uu-llc),2);
	bnd=T; if(conf2 > c2max) bnd=F;
	if(bnd) 
		{
		ms=op=c(muhat,sighat); numb=numb+1;
		bl[[numb]]=jlik(rx,ry,levs,ms,op,one23);
		} else {
			if(icbl & numb > 0) {cbl=calcblim(bl,ul); mlim=cbl$mlim; icbl=F;}
			numu=numu+1; 
			ul[[numu]]=unbd(rx,ry,levs,muhat,muhat,sighat,mlim);}
},
{	# OVERLAP (Point) (use lik)

	muhat=(m1+M0)/2; sighat=0; 
	mx=ry[rx == m1]; s1=sum(mx); s2=length(mx)-s1;
	uu=s1*log(s1) + s2*log(s2) - (s1+s2)*log(s1+s2);
	levs=uu+log(1-conf2);
	c2max=pchisq(2*(uu-llc),2);

	bnd=T; if(conf2 > c2max) bnd=F;
	if(bnd){
		op=otherpoint(rx,ry,muhat,levs,con);
		ms=c(muhat,sighat); numb=numb+1;
		bl[[numb]]=jlik(rx,ry,levs,ms,op,one23);
		} else {
			if(icbl & numb > 0) {cbl=calcblim(bl,ul); mlim=cbl$mlim; icbl=F;}
			numu=numu+1; 
			ul[[numu]]=unbd(rx,ry,levs,muhat,muhat,sighat,mlim);}
},
{	# NO OVERLAP (use lik)

	muhat=(m1+M0)/2; sighat=0; 
	uu=0;

ig=2; 
if(ig == 1) {conf2=(3+conf2)/4; levs=log(1-conf2);} else
		{c3=(3+conf2)/4; levs=log(1-c3);}

	c2max=pchisq(2*(uu-llc),2);
# above c2max=1-exp(llc), c2max --> (3+c2max)/4 implies next c2max is
	c2max=1-4*exp(llc);
	bnd=T; if(conf2 > c2max) bnd=F;
	if(bnd){
		op=c(m1,M0);
		ms=c(muhat,sighat); numb=numb+1;
		bl[[numb]]=jlik(rx,ry,levs,ms,op,one23);
		} else {
			if(icbl & numb > 0) {cbl=calcblim(bl,ul); mlim=cbl$mlim; icbl=F;}
			numu=numu+1; 
			ul[[numu]]=unbd(rx,ry,levs,M0,m1,sighat,mlim);}
}
);

}
c1max=pchisq(qchisq(c2max,2),1);

cbl=calcblim(bl,ul);
g=list(bl,ul,cbl); 
b=g[[1]]; u=g[[2]]; mlim=g[[3]]$mlim; slim=g[[3]]$slim;
plot(1,1,type="n",xlim=mlim,ylim=slim,xlab="mean (m)",ylab="standard deviation (s)");

abc=" (JLRCB)"; lin=2.7;
if(is.null(dat$tmu)) {abc=" Joint LR CB's"; lin=2.9;}
rc1=round(c1max,5); rc1=as.character(rc1); rc1=gsub("0.",".",rc1,fixed=T);
rc2=round(c2max,5); rc2=as.character(rc2); rc2=gsub("0.",".",rc2,fixed=T);
rc=rc1; irc=1; if(c1max > .99999) {rc=rc2; irc=2;}
titl1=substitute(paste(ttl0," (c1max =",rc1,")",abc,sep=""));
titl1=substitute(paste(ttl0," (max(",c[]," = ",rc1,"): ",abc,sep=""));
titl1=substitute(paste(ttl0," ",abc,sep=""))

if(!notitle)
{
mtext(titl1,side=3,line=lin,cex=1);
if(test > 2){
mtext(ttl1,side=3,line=1.5,cex=1.1,adj=0);
mtext(ttl2,side=3,line=.2,cex=1.1,adj=0);
} else mtext(tnam[test],side=3,line=.3,cex=1.1,adj=0);
}

pxl=pretty(mlim); pyl=pretty(slim); ilt=3;
abline(v=pxl,lty=ilt); abline(h=pyl,lty=ilt);
m0=-1/qnorm(con);
if(con == .5) abline(v=muhat,col=1,lty=2) else 
abline(sighat-m0*muhat,m0,col=1,lty=2);
points(muhat,sighat,pch=16,cex=.7,col=2);
if(numb > 0) {for(k in 1:numb){bk=b[[k]]; 
	lines(bk[[1]],bk[[2]],type="l");}}
if(numu > 0) {for(k in 1:numu){ uk=u[[k]]; 
	lines(uk[[1]],uk[[2]],type="l",col=2);
	lines(uk[[3]],uk[[4]],type="l",col=2);}}	
#print(vconf1)
b0=mkb0(vconf1);
b0=substitute(paste(c[]," = ",x,", ",c["max"]," = ",y,sep=""),list(x=b0,y=rc1));
b2=mkb0(vconf2);
b2=substitute(paste(c[J]," = ",x,", ",c["Jmax"]," = ",y,sep=""),list(x=b2,y=rc2));
dj=1; if(test == 1 | test == 2 | test == 5) dj=.5;
if(!notitle)
{
mtext(b0,side=3,line=1.5,cex=1,adj=dj);
if(test == 5)mtext(b2,side=3,line=.2,cex=1,adj=dj);
mtext(about,side=3,line=.4,cex=1,adj=1);
}
return(g);
}

picdat=function(dat)
{
titl=dat$title; dat=dat$d0;
# put dat$d0 into simplified form (n may not be all 1's)
dat=simp(dat);
xx=dat$X; yy=dat$Y; n=dat$COUNT;

l0=l1=numeric(0);
if(any(yy==1)){m1=min(xx[yy==1]);l1=1;}
if(any(yy==0)){M0=max(xx[yy==0]);l0=1;}

del=.025; xr=range(xx); pm=c(-1,1); xl=xr+diff(xr)*pm/100; 
del=.03;
yl=c(0,1);
par(mar=c(0,0,0,0),pin=c(2.4,1.6));
plot(xx,yy,type="n",axes=F,ylim=yl,xlim=xr,xlab="",ylab="")
lines(c(xl[1],xl[2]),c(0,0),lty=3); lines(c(xl[1],xl[2]),c(1,1),lty=3);
cx=.6;
points(xx,yy,pch=16,cex=cx)
points(m1,1,pch=16,col=2,cex=cx); points(M0,0,pch=16,col=2,cex=cx);

for(i in 1:length(xx))
{
for(j in 1:n[i]) points(xx[i],yy[i]-sign(yy[i]-.5)*(j-1)*del,pch=16,cex=cx)
}
if(l0*l1 > 0 & m1 <= M0){lines(c(m1,m1),c(0,1),lty=3); lines(c(M0,M0),c(0,1),lty=3);}
reset();
return();
}

simp=function(dat)
{
xx=dat$X; yy=dat$Y; n=dat$COUNT;
xx=rep(xx,n); yy=rep(yy,n);
sux=sort(unique(xx)); lsx=length(sux);
xxx=yyy=nnn=numeric(0);
for(i in 1:lsx)
{
i1=sum(yy[xx==sux[i]]); if(i1 > 0){xxx=c(xxx,sux[i]); yyy=c(yyy,1); nnn=c(nnn,i1);}
i0=sum(1-yy[xx==sux[i]]); if(i0 > 0){xxx=c(xxx,sux[i]); yyy=c(yyy,0); nnn=c(nnn,i0);}
}
dat=matrix(c(xxx,yyy,nnn),ncol=3); 
dat=data.frame(dat);
names(dat)=c("X","Y","COUNT");

return(dat)
}

#INDEX 109 through 113, Xlrcb1.R (5 functions)

grafl=function(limx)
	{
	lw=3;
	titl="Liklihood Ratio CL's"
	qtic=pretty(limx); qtr=range(qtic);
	pr=c(1,10,100,1000,5000,9000,9900,9990,9999)/10000; q=qnorm(pr);
	xl=range(limx); yl=c(-3.8,3.8); 
	plot(xl,yl,type="n",xlim=xl,ylim=yl,xaxt="n",yaxt="n",xlab="",ylab="",cex=.8);
	xl1=expression(paste("quantile (",italic(q),")",sep=""));
	yl1=expression(paste("Probability of Response (",italic(p),")",sep=""));
	mtext(xl1,side=1,line=1.4,cex=.9); mtext(yl1,side=2,line=2.3,cex=.8);
	mtext(titl,side=3,line=.6,cex=.8);
	isiz1=.7;
	axis(1,at=qtic,labels=T,tck=.01,cex=isiz1,mgp=c(3,.2,0));
	axis(2,at=q,labels=paste(100*pr," ",sep=""),tck=.01,cex.axis=.8,mgp=c(3,0,0),las=2);
	axis(3,at=qtic,labels=F,tck=.01,cex=isiz1);
	# Horizontal Grid Lines
	delx=.75; delx=0;
	ilt=3
	for(i in 1:length(pr))	lines(qtr,c(q[i],q[i]),lty=ilt);
	# Verticle Grid Lines
	abline(v=qtic,lty=ilt);
	del1=diff(range(limx))/20;
	return()
	}

clim0=function(rx,ry,m,s,levb)
{
done=0;
sigmax=0;
len=50;
k=5;
xll=c(-1,1)*k; 
m1=min(rx[ry==1]);M0=max(rx[ry==0]);
yll=k*(m1-M0);
if(yll == 0) yll=1;

z0=matrix(rep(0,len^2),ncol=len);

while(done == 0)
{
xl=m+xll*s; 
yl=c(sigmax,yll*s); 
x0=seq(xl[1],xl[2],length=len); y0=seq(yl[1],yl[2],length=len);
for(i in 1:len)for(j in 1:len) z0[i,j]=xyllik(rx,ry,x0[i],y0[j]);
z0=exp(z0);
cl=contourLines(x0,y0,z0,levels=levb);
ncl=length(cl);
iplot=0;
if(ncl > 0) 
{
rxcl=rycl=numeric(0);
for(i in 1:ncl) {rxcl=range(c(rxcl,cl[[i]]$x));rycl=range(c(rycl,cl[[i]]$y));}
if(iplot == 1)
{
plot(rxcl,rycl,type="n")
for(i in 1:ncl) points(cl[[i]]$x,cl[[i]]$y,type="l");
}
}

if(ncl == 0 | ncl == 4)
	{
	done=0;
	xll=1.5*xll;
	yll=1.5*yll;
	}

if(ncl == 1)
{
done=1;
x=cl[[1]]$x; y=cl[[1]]$y; en=length(x);
if(y[1] != y[en] & x[1] == xl[1]) {xll[1]=1.5*xll[1]; done=0;}
if(y[1] != y[en] & x[1] == xl[2]) {xll[2]=1.5*xll[2]; done=0;}
}

if(ncl == 2)
{
done=0;
if(rxcl[1] == xl[1]) xll[1]=1.5*xll[1];
if(rxcl[2] == xl[2]) xll[2]=1.5*xll[2];
if(rycl[2] == yl[2]) yll=1.5*yll;
}

if(ncl == 3)
{
done=0;
yll=1.5*yll;
if(rxcl[1] == xl[1]) xll[1]=1.5*xll[1];
if(rxcl[2] == xl[2]) xll[2]=1.5*xll[2];
}
}
return(c(xl,yl))
}

clim=function(rx,ry,m,s,uu,levb)
{
done=0;
sigmax=.001;
xll=c(-5,5); yll=10;
len=50;
z0=matrix(rep(0,len^2),ncol=len);

while(done == 0)
{
xl=m+xll*s; yl=c(sigmax,yll*s);
x0=seq(xl[1],xl[2],length=len); y0=seq(yl[1],yl[2],length=len);
for(i in 1:len)for(j in 1:len) z0[i,j]=xyllik(rx,ry,x0[i],y0[j])/uu;
cl=contourLines(x0,y0,z0,levels=levb);
ncl=length(cl);
if(ncl > 0)
	{
	nxl=nyl=numeric(0);
	for(i in 1:ncl){g=cl[[i]];nxl=range(c(nxl,g$x));nyl=range(c(nyl,g$y));}
	done=1;
	if(nxl[1] == x0[1]) {xll[1]=1.5*xll[1]; done=0;}
	if(nxl[2] == x0[len]) {xll[2]=1.5*xll[2]; done=0;}
	if(nyl[1] == y0[1]) {sigmax=sigmax/1.5; done=0;}
	if(nyl[2] == y0[len]) {yll=1.5*yll; done=0;}
	} else
	{
	done=0;
	xll=1.5*xll; 
	yll=yll*1.5;
	sigmax=sigmax/1.5;
	}
}
return(c(nxl,nyl))
}

abllik=function(data,mu,sig)
	{
	x=data$X;y=data$Y;n=data$COUNT;
	x=rep(x,n); y=rep(y,n);
	i1=which(y==1);
	ll=log(n[i1]*pnorm((x[i1]-mu)/sig));
	ll=c(ll,log(n[-i1]*pnorm((mu-x[-i1])/sig)));
	return(sum(ll));
	}

lrcb=function(dat,notitle=F)
{
ncl=nclu=0;
test=dat$test; ttl0=dat$ttl0; ttl1=dat$ttl1; ttl2=dat$ttl2;
tit1=dat$title; tmoo=dat$tmu;

# Compute muhat & sighat if there's overlap
dat=dat$d0; 
tnam=c("3pod","Neyer");
rx=rep(dat$X,dat$COUNT); ry=rep(dat$Y,dat$COUNT); ny=length(ry);
m1=min(rx[ry==1]);M0=max(rx[ry==0]); delq=m1-M0; 
one23=2+sign(delq);

xx="Enter conf's (separated by blanks): ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));
conf1=sort(unique(xx));
conf2=pchisq(qchisq(conf1,1),2);
if(any(conf1 <= 0) | any(conf1 >= 1)) {cat("All conf1's must be between 0 & 1\n"); return();}

if(one23 ==1)
{
xx="Enter p and q (one must be 0): ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));
} else
{
xx="Enter p: ";
xx=readline(xx); cat("\n");
xx=as.numeric(unlist(strsplit(xx," ")));
if(xx[1] <= 0 | xx[1] >= 1) {cat("p must be between 0 and 1, try again\n\n");return();}
}

# Prepare (len) X (len) Grids (z's) for Response Surface
	len=401; 	
	z0=matrix(rep(0,len*len),ncol=len);
	siglow=.001;
	meth=1;
		
# Compute muhat & sighat if there's overlap

rx=rep(dat$X,dat$COUNT); ry=rep(dat$Y,dat$COUNT); ny=length(ry);
m1=min(rx[ry==1]);M0=max(rx[ry==0]); delq=m1-M0; 
one23=2+sign(delq);

if(m1 == M0 & ny == 2){cat("More data is needed to compute valid confidence regions\n\n");return();}
if(m1 <= M0)overlap=T else overlap=F;
sigmin=.001;
if(overlap)
	{
	if(m1 < M0)
		{
		xglm=glm(ry~rx,family=binomial(link=probit));	
		ab=xglm$coef;
		sighat=1/ab[2];
		muhat=-ab[1]*sighat;
		uu=xyllik(rx,ry,muhat,sighat)
		} else
		{
		muhat=m1; sighat=sigmin; mx=ry[rx == m1]; s1=sum(mx); l1=length(mx);
		uu=s1*log(s1)+(l1-s1)*log(l1-s1)-l1*log(l1);
		}
	} else 
		{
		denom=2;
		muhat=(m1+M0)/2; sighat=(m1-M0)/denom;  
		nconf2=(3+pchisq(qchisq(conf1,1),2))/4;
		uu=1;
		}

#(qq,pp) is a point on MLE RESPONSE CURVE
if(xx[1] > 0 & xx[1] < 1) {pp=xx[1]; qq=muhat+qnorm(pp)*sighat;}
if(one23 == 2) qq=muhat;
if(xx[1] == 0 & one23 == 1) {qq=xx[2]; pp=pnorm((qq-muhat)/sighat);}

nobo=F; pcl=T;
# Rough calculation of limits
if(overlap)
{
	levs=1-qchisq(conf1,1)/(2*uu);
	con=sum(ry)/length(ry); 
	llc=sum(log(con^ry*(1-con)^(1-ry)));
	c1max=pchisq(2*(uu-llc),1);
	bcon1=conf1[conf1 < c1max];
	ucon1=conf1[conf1 >= c1max];
	nu1=length(ucon1);
	endpr=paste("Overlap: All conf1's are > c1max (",round(c1max,5),")\n\n",sep="");
	# Address all unbounded contours
	if(length(bcon1) == 0)  
		{
		cat(endpr); 
		nobo=T;
		}
	if(!nobo) bconm=max(bcon1,na.rm=T) else bconm=c1max/2;;
	levm=1-qchisq(bconm,1)/(2*uu);
	a=clim(rx,ry,muhat,sighat,uu,levm);
} else
	{
	uu=1;
	levs=(1-conf2)/4;
	con=sum(ry)/length(ry); 
	lc=prod(con^ry*(1-con)^(1-ry));
	c2max=1-4*lc; 
	c1max=pchisq(qchisq(c2max,2),1);
	bcon2=conf2[conf2 < c2max];
	ucon2=conf2[conf2 >= c2max];
	nu1=length(ucon2);
	endpr=paste("No Overlap: All conf1's are > c1max (",round(c1max,5),")\n\n",sep="");
	# Address all unbounded contours
	if(length(bcon2) == 0)  
		{
		cat(endpr); 
		nobo=T; pcl=F;
		}
	if(!nobo) bconm=max(bcon2,na.rm=T) else bconm=c2max/2;
	levm=(1-bconm)/4; 
	a=clim0(rx,ry,muhat,sighat,levm);
}

# Expand limits a tad
sigmax=0;
a1=c(floor(a[1]), ceiling(a[2]), min(sigmax, .1*a[3]), ceiling(a[4]))+c(-1,1,0,1);
x0=seq(a1[1],a1[2],length=len); y0=seq(a1[3],a1[4],length=len);
if(meth == 1){for(i in 1:len)for(j in 1:len)	z0[i,j]=xyllik(rx,ry,x0[i],y0[j])/uu;}
if(meth==2){for(i in 1:len)for(j in 1:len)	z0[i,j]=uu-xyllik(rx,ry,x0[i],y0[j]);}
if(!overlap)z0=exp(z0);

# Neyer CL's provided on his (Mu,Sig) contour plot
# Levels of z0 relate to conf by: -2 * log( exp(xyllik)/exp(uu) ) >= qchisq(conf,2)
# levs=1-qchisq(conf2,2)/(2*uu); cl=contourLines(x0,y0,z0,levels=levs);

if(!nobo) {if(overlap)levb=1-qchisq(bcon1,1)/(2*uu) else levb=(1-bcon2)/4;}
if(nobo) {if(overlap)levb=1-qchisq(bconm,1)/(2*uu) else levb=(1-bconm)/4;}

cl=contourLines(x0,y0,z0,levels=levb); ncl=length(cl);

nxl=nyl=numeric(0);

if(!nobo) {for(i in 1:ncl){g=cl[[i]];nxl=range(c(nxl,g$x));nyl=range(c(nyl,g$y));}}

if(nu1 > 0)
	{
	if(overlap) levu=1-qchisq(ucon1,1)/(2*uu) else levu=(1-ucon2)/4
	clu=contourLines(x0,y0,z0,levels=levu);
	nclu=length(clu);
	if(nclu > 0)for(i in 1:nclu){g=clu[[i]];nxl=range(c(nxl,g$x));nyl=range(c(nyl,g$y));}
	}

nco=length(conf1);
#-----------------------------------------------------------------------------
# Limits for plot 
xl=yl=numeric(0);
if(!nobo)for(i in 1:ncl){g=cl[[i]];xl=range(c(xl,g$x));yl=range(c(yl,g$y));}

if(nclu > 0) {for(i in 1:nclu){g=clu[[i]]; 
xl=range(c(xl,g$x));
yl=range(c(yl,g$y));
}}

if(!pcl & nclu > 0)
{
xl=yl=numeric(0);
for(i in 1:nclu){g=clu[[i]];xl=range(c(xl,g$x));yl=range(c(yl,g$y));}
}

xll=c(floor(xl[1]),ceiling(xl[2]));
yll=c(floor(yl[1]),ceiling(yl[2]));
pxl=pretty(xl); pyl=pretty(yl);

par(mfrow = c(2, 2), oma = c(0,.4,2,0),  mar = c(3,3,2,1));
#-----------------------------------------------------------------------------
ex=cl[[1]]$x; ey=cl[[1]]$y;

dtp="l"; if(nobo) dtp="n";
plot(ex,ey,type=dtp,xlim=xl[1:2],ylim=yl[1:2],xlab="",ylab="",xaxt="n",yaxt="n");
ilt=3; abline(v=pxl,lty=ilt); abline(h=pyl,lty=ilt);
points(muhat,sighat,pch=16,cex=.7,col=2);
if(nu1 > 0)
	{
	for(i in 1:length(clu)){exx=clu[[i]]$x; eyy=clu[[i]]$y; points(exx,eyy,type="l",col=2);}
	}

axis(1,at=pxl,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pyl,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext("mean (m)",side=1,line=1.3,cex=.9);
mtext("standard deviation (s)",side=2,line=2.5,cex=.9);

if(ncl > 1 & !nobo)for(i in 1:ncl){ex=cl[[i]]$x; ey=cl[[i]]$y; points(ex,ey,type="l");}

b0=mkb0(conf2);
mtext(substitute(paste(c[J],"=",x,sep=""),list(x=b0)),side=3,line=.6,cex=.8);

con=sum(ry)/length(ry); llc=sum(log(con^ry*(1-con)^(1-ry)));
m0=-1/qnorm(con);
if(con == .5) abline(v=muhat,col=1,lty=2) else abline(sighat-m0*muhat,m0,col=1,lty=2);
rngx=xl; rngy=yl;

#-----------------------------------------------------------------------------
# Limits for plot 2
xl=numeric(0);
for(i in 1:ncl){g=cl[[i]];xl=range(c(xl,g$x+qnorm(pp)*g$y),finite=T);}

if(!pcl & nclu > 0)
{
xl=numeric(0);
for(i in 1:nclu){g=clu[[i]];xl=range(c(xl,g$x+qnorm(pp)*g$y));}
}

xll=c(floor(xl[1]),ceiling(xl[2]));
pxl2=pretty(xl);
#-----------------------------------------------------------------------------
ex=cl[[1]]$x+qnorm(pp)*cl[[1]]$y; 
ey=cl[[1]]$y;

rng2=range(ex,finite=T);

plot(ex,ey,type=dtp,xlim=xl,ylim=yl[1:2],xlab="",ylab="",xaxt="n",yaxt="n");
ilt=3; abline(v=pxl2,lty=ilt); abline(h=pyl,lty=ilt);
points(qq,sighat,pch=16,cex=.7,col=2);

axis(1,at=pxl2,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pyl,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext(expression(paste("quantile (",italic(q),")",sep="")),side=1,line=1.4,cex=.9);

if(ncl > 1 & pcl)for(i in 1:ncl){ex=cl[[i]]$x+qnorm(pp)*cl[[i]]$y;; ey=cl[[i]]$y; points(ex,ey,type="l");}
if(nu1 > 0)for(i in 1:length(clu)){exx=clu[[i]]$x+qnorm(pp)*clu[[i]]$y;; eyy=clu[[i]]$y; points(exx,eyy,type="l",col=2);}

b1=mkb0(conf1);
rpp=xlead0(pp,3);

b2=substitute(paste(italic(p),"=",x,", ",c[],"=",y,sep=""), list(x=rpp,y=b1))
mtext(b2,side=3,line=.6,cex=.8);

if(pp == con) abline(v=muhat+qnorm(pp)*sighat,col=1,lty=2) else
	{ 
	m3=1/(qnorm(pp)+1/m0);
	abline(sighat-m3*(muhat+qnorm(pp)*sighat),m3,col=1,lty=2);
	}

#-----------------------------------------------------------------------------
#Limits for plot 3
xl=numeric(0);

for(i in 1:ncl){g=cl[[i]];xl=range(c(xl,pp,pnorm((qq-g$x)/g$y)));}
if(nclu > 0) {for(i in 1:nclu){g=clu[[i]];xl=range(c(xl,pp,pnorm((qq-g$x)/g$y)));}}

xll=c(floor(xl[1]),ceiling(xl[2]));
pxl3=pretty(xl);
#-----------------------------------------------------------------------------
ex=pnorm((qq-cl[[1]]$x)/cl[[1]]$y); 
ey=cl[[1]]$y;

plot(ex,ey,type=dtp,xlim=xl,ylim=yl[1:2],xlab="",ylab="",xaxt="n",yaxt="n");
points(pp,sighat,pch=16,cex=.7,col=2);
ilt=3; abline(v=pxl3,lty=ilt); abline(h=pyl,lty=ilt);

axis(1,at=pxl3,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pyl,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext(expression(paste("probability (",italic(p),")",sep="")),side=1,line=1.4,cex=.9);
mtext("standard deviation (s)",side=2,line=2.5,cex=.9);

if(ncl > 1 & pcl)for(i in 1:ncl){ex=pnorm((qq-cl[[i]]$x)/cl[[i]]$y); ey=cl[[i]]$y; points(ex,ey,type="l");}
if(nu1 > 0)for(i in 1:length(clu)){exx=pnorm((qq-clu[[i]]$x)/clu[[i]]$y); eyy=clu[[i]]$y; points(exx,eyy,type="l",col=2);}

rmu=xlead0(muhat,3); rqq=xlead0(qq,3);
if(overlap) b2=substitute(paste("q=",rqq,", ",c[],"=",b1,sep="")) else
b2=substitute(paste("q=",rmu,", ",c[],"=",b1,sep=""))

mtext(b2,side=3,line=.6,cex=.8);
abline(v=con,col=1,lty=2);
#-----------------------------------------------------------------------------
# linearized probability plot (for a single contour cl[[icl]])
{
al=10^(6:2); al=c(1/al,1-1/al); al=c(al,seq(25,975,by=25)/1000); al=sort(al);
nal=length(al);
lrmat1=matrix(rep(0,6*nal),ncol=6);
lrmat1[,5]=al;
lrmat1[,2]=muhat+qnorm(al)*sighat;
lra=list(1);
limx=numeric(0);
if(nobo) ncl=0;
for(j in 1:(ncl+nclu))
{
if(j > ncl) {x=clu[[j-ncl]]$x; y=clu[[j-ncl]]$y;} else {x=cl[[j]]$x; y=cl[[j]]$y;}
for(i in 1:nal)
	{
	lrmat1[i,c(1,3)]=lim=range(x+qnorm(lrmat1[i,5])*y,finite=T);
	if(i > 2 & i < 48) limx=range(c(limx,lim));
	lrmat1[i,c(4,6)]=range(pnorm((lrmat1[i,2]-x)/y),finite=T);
	}
lra[[j]]=lrmat1
}

abc=" LR CB's";
if(c1max > 0) c2max=pchisq(qchisq(c1max,1),2) else c2max = 0;
rc1=xlead0(c1max,5); rc2=xlead0(c2max,5);

if(ncl > 0) grafl(limx) else
{
# Null Plot Just for Text
plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
titt1="LR CL's Do Not Exist";
titt2="Requirements Are:";
titt3="Overlap, and"
mtext(titt1,side=3,line=-4.6,cex=.9);
mtext(titt2,side=3,line=-6.6,cex=.9);
mtext(titt3,side=3,line=-8.6,cex=.9);
titt4=substitute(paste(c[1]," < max(",c[1],") = ",rc1,sep=""))
mtext(titt4,side=3,line=-10.6,cex=.9);
}

if(ncl > 0)
{
if(one23 > 1) abline(v=muhat,col=8)

for(j in (ncl):1)
{
u=lra[[j]]; 
if(j > ncl) pcol=2 else pcol=1; 
if(j <= ncl | (j-ncl)%%2 ==1)lines(u[,1],qnorm(u[,5]),type="l",col=pcol);
if(j <= ncl | (j-ncl)%%2 ==0)lines(u[,3],qnorm(u[,5]),type="l",col=pcol);
}

if(one23==1) lines(u[,2],qnorm(u[,5]),type="l",col=8)

}

cn=c("ql","q","qh","pl","p","pu");
options(scipen=999);
write.table(round(lra[[1]],6),file="lrcb.txt",quote=F,sep=",",na="i",
col.names=cn,row.names=F);
if(ncl > 1)for(j in 2:ncl)
{
suppressWarnings(write.table(round(lra[[j]],6),file="lrcb.txt",quote=F,sep=",",na="i",append=T,
col.names=cn,row.names=F));
}
options(scipen=0);
}
# main title
par(mfrow=c(1,1));
par(oma = c(0,0,1,0),  mar = c(5,4,4,2)+.1);

if(c1max < .999995) tit7=substitute(paste(abc,", ",c["max"]," = ",rc1,sep="")) else
                    tit7=substitute(paste(abc,", ",c["Jmax"]," = ",rc2,sep=""))

if(!notitle){
if(!is.null(tmoo)) mtext(substitute( paste(ttl1," ",ttl2,", ",ttl0,", ",tit7,sep="")),side=3,line=-.6,cex=1,outer=T) else
mtext(substitute(paste(tit1,", ",tit7,sep="")),side=3,line=-.6,cex=1,outer=T)
}

return();
}

#INDEX 114 through 118 Xcbs2.R (5 functions)

mdose.p=function(obj,p)
	{
	np=length(p)
	se=rep(0,np)
	b=as.vector(obj$coef)
	x.p=(qnorm(p)-b[1])/b[2]
	for(i in 1:np)
	{
	pd= -c(1,x.p[i])/b[2]
	se[i]=sqrt((t(pd)%*%vcov(obj))%*%pd)
	}
	a=matrix(c(x.p,se),ncol=2)
	a=data.frame(a)
	names(a)=c("dose","se")
	return(a)
	}

mixed=function(dat)
{
	min1=min(dat$X[which(dat$Y==1)]);
	max0=max(dat$X[which(dat$Y==0)]);
	dif=min1-max0;		# dif < 0 <=> OVERLAP or ZONE OF MIXED RESULTS
	summ=min1+max0;
	con=sum(dat$Y)/length(dat$Y);
	result=list(min1,max0,summ,dif,con)
	names(result)=c("min1","max0","summ","dif","con");
	return(result); 
}

graf1=function(limx,t1,k,big,legnd)
	{
	lw=3;
	spl="SPlus (dose.p)"; if(k!=1)spl="SPlus (GLM)";	if(big==0)sp1="SP";
	leg=c("FM","LR","GLM"); if(big!=0)leg=c("Fisher Matrix","Likelihood Ratio",spl);
	qtic=pretty(limx);
	pr=c(1,10,100,1000,5000,9000,9900,9990,9999)/10000; q=qnorm(pr);
	xl=range(limx); yl=c(-3.8,3.8); 
	plot(xl,yl,type="n",xlim=xl,ylim=yl,xaxt="n",yaxt="n",xlab="",ylab="",cex=.8);
	xl1=expression(paste("quantile (",italic(q),")",sep=""));
	yl1=expression(paste("Probability of Response (",italic(p),"%)",sep=""));
	mtext(xl1,side=1,line=1.6,cex=.8); mtext(yl1,side=2,line=2.3,cex=.8);
	mtext(t1,side=3,line=.6,cex=.9);
	isiz1=.7;if(big==0)isiz1=.4;
	axis(1,at=qtic,labels=T,tck=.01,cex=isiz1,mgp=c(3,.5,0));
	axis(2,at=q,labels=paste(100*pr," ",sep=""),tck=.01,cex.axis=.8,mgp=c(3,0,0),las=2);
	axis(3,at=qtic,labels=F,tck=.01,cex=isiz1);
	# Horizontal Grid Lines
	delx=.75
	ilt=3
	for(i in 1:length(pr))	lines(xl+c(-delx,delx),c(q[i],q[i]),lty=ilt);
	# Verticle Grid Lines
	abline(v=qtic,lty=ilt);
	del1=diff(range(limx))/20;
	if(legnd>0)legend(xl[1]+del1,qnorm(.997),legend=leg,lty=c(1,1,1),col=c(4,1,2),lwd=lw,cex=.6,bg="white");
	return()
	}

lrmax=function(w,plt=F)
{
dat=w$d0; title=w$title;

rx=dat$X; ry=dat$Y; nc=dat$COUNT;
rx=rep(rx,nc); ry=rep(ry,nc); 
nt=sum(nc);
con=sum(ry)/length(ry);
llc=sum(log(con^ry*(1-con)^(1-ry)));
r0=rx[ry==0]; r1=rx[ry==1]; 
mix=length(r0)*length(r1);
lux=length(unique(rx));
flg=0;
if(mix == 0 | lux == 1)	flg=1;
M0=max(r0); m1=min(r1); del=m1-M0; one23=2+sign(del);

switch(one23,
{	# OVERLAP (Interval) (use log lik)
	xglm=glm(ry~rx,family=binomial(link=probit));	
	ab=as.vector(xglm$coef);
	muhat=-ab[1]/ab[2]; sighat=1/ab[2];
	uu=xyllik(rx,ry,muhat,sighat);
},
{	# OVERLAP (Point) (use lik)
	muhat=(m1+M0)/2; sighat=0; 
	mx=ry[rx == m1]; s1=sum(mx); s2=length(mx)-s1;
	uu=s1*log(s1) + s2*log(s2) - (s1+s2)*log(s1+s2);
},
{	# NO OVERLAP (use lik)
	muhat=(m1+M0)/2; sighat=0; uu=0; 
}
);

c2max=pchisq(2*(uu-llc),2);
c1max=pchisq(qchisq(c2max,2),1);
a=c(con,llc,c1max,c2max);

con=round(con,5); llc=round(llc,5); 
c1max=round(c1max,5); c2max=round(c2max,5);
wx=list(dat,title,one23,con,llc,c1max,c2max,flg);
names(wx)=c("d0","title","one23","con","llc","c1max","c2max","flg")
if(plt) picdat(wx);
return(wx)
}

cbs=function(w,plt,notitle=F)
{
# gs for graph sheet 1 or 2
# gs = 1 with neither pp nor qq ==> jms = 1 for (mu sig contour plot)
# gs = 1 with a valid pp or qq ==> jms = 3 for 3 plots on one
# gs = 2

fmmat=spmat=lrmat=matrix(rep(NA,6*15),ncol=6);
mat=matrix(rep(NA,3*11*6),ncol=6);

a1=c(1,10,100,1000,10000,100000,250000)/1000000; al=c(a1,.5,sort(1-a1));

gs=0;
if(plt == 1 | plt == 7) gs=1;
if(plt == 2 | plt == 8) gs=2;
if(gs == 0) return();

if(gs == 1)
{
	cflag=0;
	xx="Enter conf and Jconf (one must be 0): ";
	xx=readline(xx); cat("\n");
	xx=as.numeric(unlist(strsplit(xx," ")));
	if(length(xx) != 2) cflag=-1;
	if(cflag == 0)
	{
	if(xx[1]>0 & xx[1]<1){cflag=1; conf1=xx[1];}
	if(cflag == 0 & xx[2]>0 & xx[2]<1){cflag=2; conf2=xx[2];}
	if(cflag == 1) {conf2=pchisq(qchisq(conf1,1),2); cn="c1";}
	if(cflag == 2) {conf1=pchisq(qchisq(conf2,2),1); cn="c2";}
	}
	if(cflag <= 0) return()

	pflag=0; q0=.001;
	xx="Enter p and q (at least one must be 0): ";
	xx=readline(xx); cat("\n");
	xx=as.numeric(unlist(strsplit(xx," ")));
	if(length(xx) != 2) return();
	if(pflag == 0)
	{
	if(xx[1]>0 & xx[1]<1) {pflag = 1; pp=xx[1];}
	if(pflag == 0 & xx[1] == 0 & xx[2] != 0) {qq=xx[2]; pflag = 2;}
	if(pflag == 2 & xx[2] == q0) xx[2]=0;
	}
	if(pflag == 0) jms=1 else jms=3;
} else
{
	# jms must be defined here, as it's used in an if test later
	cflag=0; jms=0; 
	xx="Enter conf: ";
	xx=readline(xx); cat("\n");
	xx=as.numeric(unlist(strsplit(xx," ")));
	if(length(xx) != 1 | xx[1] <= 0 | xx[1] >= 1) return();
	conf1=xx[1]; conf2=pchisq(qchisq(conf1,1),2);
}
	g=lrmax(w); c1max=g$c1max; c2max=g$c2max; one23=g$one23; flg=g$flg;
	if(flg == 1) {cat(paste("Need to do more testing\n",sep="")); return();}
	if(one23 > 1) {cat(paste("Need interval overlap for this particular plot.\n\n",sep="")); return(); } else
	if(conf1 >= c1max & (gs < 3)) {cat(paste("Need conf1 < ",round(c1max,4),", for this particular plot: Try again.\n\n",sep="")); return();}
	see1=xlead0(conf1,3);	
	dat=w$d0; tit0=w$titl; ttl0=w$ttl0; ttl1=w$ttl1; ttl2=w$ttl2; tmoo=w$tmu;

# do LR (needed if gs = 1 or 2) if conf1 < c1max and FM & SP (if gs = 2 and dif < 0)

	if(conf1 < c1max)
	{
	a=lrq.lims(dat,conf1,P=al);
	cx=a$cx; cy=a$cy; lrmat=a$lrmat; dif=a$dif; con=a$con;
	muhat=a$muhat; sighat=a$sighat; ab=c(muhat,sighat);
	if(dif >= 0) {conf2=(conf2+3)/4; ab=c(muhat,Inf);}
	}

	if(gs == 1 & jms == 3)
	{
	if(pflag == 1) qq=muhat+qnorm(pp)*sighat else pp=pnorm((qq-muhat)/sighat);	
	}

if(dif<0)fmmat= fm.lims(dat,conf1,P=al);
if(dif<0)spmat=glm.lims(dat,conf1,P=al);
cf=xlead0(c(conf1,conf2),4); 
rc=xlead0(c(c1max,c2max),4);
titt3=substitute(paste(c[]," = ",u,", ",c["max"]," = ",v,sep=""),list(u=cf[1],v=rc[1]));

titt4=substitute(paste(c[J]," = ",u,", ",c["Jmax"]," = ",v,sep=""),list(u=cf[2],v=rc[2]));

#===========================Graph Sheet 1 (Graphs 1, 2 and 3)=============================
# To get: just graphsheet 1, set gs=1; just graphsheet 2, set gs=2.
options(warn=-1); # SUPPRESSES OUT OF BOUNDS WARNINGS (AND OTHERS)
isiz=0.9; rd=6;
#=========================================Graph 1========================================

rd=4;
ilt=3;

rx=range(cx); ry=range(cy); prx=pretty(rx); pry=pretty(ry);

if(gs == 1)
{
if(jms == 3) {par(mfrow = c(2, 2), oma = c(0,.4,1.5,0),  mar = c(3,3,3,1)); isiz=0.9;}
plot(cx,cy,type="l",xlim=rx,ylim=ry,xlab="",ylab="",xaxt="n",yaxt="n");
axis(1,at=prx,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pry,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext("mean (m)",side=1,line=1.3,cex=.9);
mtext("standard deviation (s)",side=2,line=2.5,cex=.9);
abline(v=prx,lty=ilt); abline(h=pry,lty=ilt);
points(muhat,sighat,pch=16,cex=.7,col=2);
cf=xlead0(rx,4); tit2a=paste(cf[1]," < m < ",cf[2],sep="");
cf=xlead0(ry,4); tit2b=paste(cf[1]," < s < ",cf[2],sep="");
ndj=.5; if(jms == 1) ndj=0;
mtext(tit2a,side=3,line=1.4,cex=.9,adj=ndj);
mtext(tit2b,side=3,line=0.2,cex=.9,adj=ndj);
m0=-1/qnorm(con);
if(con == .5) abline(v=muhat,col=1,lty=2) else abline(sighat-m0*muhat,m0,col=1,lty=2);

#=========================================================================================

if(jms == 3)
{
#=========================================Graph 2========================================
rx=range(cx+qnorm(pp)*cy); prx=pretty(rx);
plot(cx+qnorm(pp)*cy,cy,type="l",xlim=rx,ylim=ry,xlab="",ylab="",xaxt="n",yaxt="n");
axis(1,at=prx,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pry,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext(expression(paste("quantile (",italic(q),")",sep="")),side=1,line=1.3,cex=.9);
points(qq,sighat,pch=16,cex=.7,col=2);
abline(v=pretty(rx),lty=ilt); abline(h=pretty(ry),lty=ilt);
cf=xlead0(pp,4);
tit1=substitute(paste(italic(p)," = ",cf, sep=""))
tit2=substitute(paste(x," < ",italic(q)," < ", y, sep=""), list(x=round(rx[1],rd),y=round(rx[2],rd)))
mtext(tit1,side=3,line=1.4,cex=.9);
mtext(tit2,side=3,line=0.2,cex=.9);
if(pp == con) abline(v=muhat+qnorm(pp)*sighat,col=1,lty=2) else
	{ 
	m3=1/(qnorm(pp)+1/m0);
	abline(sighat-m3*(muhat+qnorm(pp)*sighat),m3,col=1,lty=2);
	}
#==========================Graph 3 (Null Plot Just for Text)============================

plot(c(0,1),c(0,1),type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
mtext(titt3,side=3,line=-4,cex=.9,adj=0);
mtext(titt4,side=3,line=-7,cex=.9,adj=0);

#=========================================Graph 4========================================

rx=range(pnorm((qq-cx)/cy)); prx=pretty(rx);
plot(pnorm((qq-cx)/cy),cy,xlab="",ylab="",type="l",xlim=rx,ylim=ry,xaxt="n",yaxt="n");
axis(1,at=prx,labels=T,tck=.01,cex=.8,mgp=c(3,.2,0));
axis(2,at=pry,labels=T,tck=.01,mgp=c(3,.2,0),las=2);
mtext(expression(paste(italic(p),sep="")),side=1,line=1.3,cex=.9);
mtext("standard deviation (s)",side=2,line=2.5,cex=.9);
points(pp,sighat,pch=16,cex=.7,col=2);
abline(v=pretty(rx),lty=ilt); abline(h=pretty(ry),lty=ilt);
abline(v=con,col=1,lty=2);
cf=xlead0(qq,4); tit1=substitute(paste(italic(q)," = ",cf, sep=""));
cf=xlead0(rx,4);
tit2=substitute(paste(x," < ",italic(p)," < ", y, sep=""), list(x=cf[1],y=cf[2]))
mtext(tit1,side=3,line=1.4,cex=.9);
ds=0;
mtext(tit2,side=3,line=0.2-ds,cex=.9);
}

par(mfrow=c(1,1));par(oma = c(0,0,1,0),  mar = c(5,4,4,2)+.1);
if(jms != 3)
{
mtext(titt3,side=3,line=2.2,cex=.9,adj=1);
mtext(titt4,side=3,line=1,cex=.9,adj=1);
}

if(!notitle) {
if(!is.null(tmoo)) mtext(substitute(paste(ttl1," ",ttl2,", ",ttl0,", LR CB's",sep="")),side=3,line=-.5,cex=1,outer=T) else
mtext(substitute(paste(tit0,", LR CB's",sep="")),side=3,line=-.5,cex=1,outer=T)
}

}

# Done with graph sheet 1 (gs = 1) containing either graph 1 or graphs 1, 2 3 and 4

#=======Define stuff needed for Graph Sheet 2 (next 4 plots) define lrmat,fmmat,spmat=======

if(gs == 2)
{
par(mfrow = c(2, 2), oma = c(0,0,1,0),  mar = c(3,4,3,1));
tq2="(qlo,qhi) about q (given p)";	tp2="(plo,phi) about p (given q)";
tq1="(qlo,qhi) about q (supressing p)";	tp1="(plo,phi) about p (supressing q)";

tq2=expression(paste("(",italic(q)[lo],",",italic(q)[hi],") about ",italic(q)," (given ",italic(p),")",sep=""));
tp2=expression(paste("(",italic(p)[lo],",",italic(p)[hi],") about ",italic(p)," (given ",italic(q),")",sep=""));
tq1=expression(paste("(",italic(q)[lo],",",italic(q)[hi],") about ",italic(q)," (supressing ",italic(p),")",sep=""));
tp1=expression(paste("(",italic(p)[lo],",",italic(p)[hi],") about ",italic(p)," (supressing ",italic(q),")",sep=""));

# To plot Linearized Response, and CL's versus q
# For Graph Sheet 2, need Range & delta on q-axis (qmin,qmax,bi) to cover FM, LR, and dose.p qlo's and qhi's 

# Limits for the q axis for the two graphs using the function graf1
big=0; 
isiz=.8;
legq=c("Likelihood Ratio","Fisher Matrix","SPlus (dose.p)");
legp=c("Likelihood Ratio","Fisher Matrix","SPlus (GLM)");	
if(big==0){par(mfrow=c(2,2));isiz=.5; legq=legp=c("LR","FM","GLM")};
if(big!=0)	par(mfrow=c(1,1));

# pr (graf1) is of length 15. For purposes of graphs 4 & 6, 
# Skip 1st two & last 2 two - corresponding to 1/million, 1/100000, 99999/10000, 999999/1000000

ih=3:13;

if(dif < 0) 
{
if(conf1 < c1max) {mat[1:11,]=lrmat[ih,]; mat[12:22,]=fmmat[ih,]; mat[23:33,]=spmat[ih,]; i3=3;} else
{mat[12:22,]=fmmat[ih,]; mat[23:33,]=spmat[ih,]; i3=2;}
} else
{
if(conf1 < c1max) {mat[1:11,]=lrmat[ih,]; i3=1;}
}
jj4=range(c(mat[,1],mat[,3]),na.rm=T);

#======================Graph 5 (Linearized Response with (qlo,qhi))=======================
# FM Color = 4 (Blue); LR Color = 1 (Black); SP Color = 2 (RED)
fmc=4; lrc=1; spc=2;

	graf1(jj4,tq2,1,big,1);
	lw=1.9;
# LCL Curves and UCL Curves about q(p)
if(i3 == 1 | i3 == 3)
{
w=lrmat;cl=lrc;
lines(w[,1],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,3],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,5]),type="l",col=8,cex=2,lwd=lw);
}
if(i3 == 2 | i3 == 3)
{
w=fmmat;cl=fmc;
lines(w[,1],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,3],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
w=spmat;cl=spc;
lines(w[,1],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,3],qnorm(w[,5]),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,5]),type="l",col=8,cex=2,lwd=lw);
}

#==========================Graph 6 (qlo & qhi versus q  Plots)============================

ih=1:15
if(i3 == 2 | i3 == 3) jj5=range(c(lrmat[ih,2],fmmat[ih,2],spmat[ih,2]),na.rm=T) else jj5=muhat+c(-1,1);
plot(lrmat[,2],lrmat[,3],xlim=range(jj5),ylim=range(pretty(jj4)),type="n",xlab="",ylab="",xaxt="n",yaxt="n");

mtext(expression(paste("quantile (",italic(q),")",sep="")),side=1,line=1.6,cex=.8);
mtext(expression(paste(italic(q[lo])," & ",italic(q[hi]),sep="")),side=2,line=2,cex=.8);
qtic=pretty(jj5);
axis(1,at=qtic,labels=T,tck=.01,cex=.8,mgp=c(3,.5,0));
qtic=pretty(jj4);
axis(2,at=qtic,labels=T,tck=.01,cex=.8,mgp=c(3,.5,0));

box();
abline(v=pretty(jj5),lty=ilt);
abline(h=pretty(jj4),lty=ilt);

if(i3 == 2 | i3 == 3)
{
lines(fmmat[,2],fmmat[,3],col=fmc,lwd=lw);lines(lrmat[,2],lrmat[,3],col=lrc,lwd=lw);lines(spmat[,2],spmat[,3],col=spc,lwd=lw);
lines(fmmat[,2],fmmat[,1],col=fmc,lwd=lw);lines(lrmat[,2],lrmat[,1],col=lrc,lwd=lw);lines(spmat[,2],spmat[,1],col=spc,lwd=lw);
lines(lrmat[,2],lrmat[,2],col=8,lwd=lw);
} 

if(i3 == 1 | i3 == 3) 
{
lines(lrmat[,2],lrmat[,3],col=lrc,lwd=lw);
lines(lrmat[,2],lrmat[,1],col=lrc,lwd=lw);
lines(lrmat[,2],lrmat[,2],col=8,lwd=lw);
}
mtext(tq1,side=3,line=.6,cex=.9);

#=====================Graph 7 (Linearized Response with (plo,phi))========================

	graf1(jj5,tp2,2,big,0);
	
# LCL Curves and UCL Curves about p(q)
	ep0=0.000001; ep1=1-ep0;
		
if(i3 == 1 | i3 == 3)
{
w=lrmat;cl=lrc;
lines(w[,2],qnorm(w[,4]+ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,6]-ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,5]),type="l",col=8,cex=2,lwd=lw);
}
if(i3 == 2 | i3 == 3)
{
w=fmmat;cl=fmc;
lines(w[,2],qnorm(w[,4]+ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,6]-ep0),type="l",col=cl,cex=2,lwd=lw);

w=spmat;cl=spc;
lines(w[,2],qnorm(w[,4]+ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,6]-ep0),type="l",col=cl,cex=2,lwd=lw);
lines(w[,2],qnorm(w[,5]),type="l",col=8,cex=2,lwd=lw);
}
	
#==========================Graph 8 (plo & phi versus p  Plots)============================

h=100;
plot(h*lrmat[,5],h*lrmat[,6],xlim=h*c(0,1),ylim=h*c(0,1),type="n",xlab="",ylab="",xaxt="n",yaxt="n");
mtext(expression(paste("probability ",italic(p)," (in %)",sep="")),side=1,line=1.6,cex=.8);
mtext(expression(paste(italic(p)[lo]," & ",italic(p)[hi]," (in %)",sep="")),side=2,line=2,cex=.8);
qtic=pretty(c(0,100));
axis(1,at=qtic,labels=T,tck=.01,cex=.8,mgp=c(3,.5,0));
axis(2,at=qtic,labels=T,tck=.01,cex=.8,mgp=c(3,.5,0));

abline(v=pretty(h*c(0,1)),lty=ilt);abline(h=pretty(c(0,100)),lty=ilt);

if(i3 == 2 | i3 == 3)
{	
lines(h*fmmat[,5],h*fmmat[,6],col=fmc,lwd=lw);lines(h*spmat[,5],h*spmat[,6],col=spc,lwd=lw);
lines(h*fmmat[,5],h*fmmat[,4],col=fmc,lwd=lw);lines(h*spmat[,5],h*spmat[,4],col=spc,lwd=lw);
}
if(i3 == 1 | i3 == 3)
{
lines(h*lrmat[,5],h*lrmat[,6],col=lrc,lwd=lw);
lines(h*lrmat[,5],h*lrmat[,4],col=lrc,lwd=lw);
}
box();
lines(c(0,100),c(0,100),lwd=lw,col=8);
mtext(tp1,side=3,line=.6,cex=.9);
par(mfrow=c(1,1));
par(oma = c(0,0,1,0),  mar = c(5,4,4,2)+.1);
c1=round(100*conf1,2);
if(!notitle) {
if(!is.null(tmoo)) mtext(substitute(paste(ttl1," ",ttl2,", ",ttl0,", ",c[],"=",see1,sep="")),side=3,line=-.5,cex=1,outer=T) else
mtext(substitute(paste(tit0,", ",c[],"=",see1,sep="")),side=3,line=-.5,cex=1,outer=T) 
}

#==========================================================================================
}

matt=rbind(fmmat,lrmat,spmat);
cn=c("ql","q","qh","pl","p","pu");
options(scipen=999);
rmatt=round(matt,6);
write.table(rmatt,file="cbs.txt",quote=F,sep=",",na="i",col.names=cn,row.names=F)
reset();
return(rmatt)
}

#INDEX 119, wxdat (1 function)

wxdat=function(ic,plt=T)
{
ad=""; plt0=plt;

if(!is.element(ic,1:27)) {cat(paste("\nArgument to wxdat must be between 1 and 27 inclusive\n")); return();}

switch(ic,
		{	# 1
			titl1="My SenTest Ex.";
			xx=c(13,15,14,15.814,14.5,13.2197,15.0273,13.6665); 
			yy=c(0,1,0,1,1,0,1,1);
			n=rep(1,length(xx));
 		},
		{	# 2
			titl1="MIL-STD-331 Ex.";
			xx=c(1.00,1.20,1.40,1.80,2.60,4.20,3.40,3.80,4.00,4.10,
			4.28,4.52,5.55,5.24,6.37,6.08,7.38,7.09,6.89,6.74);
			yy=c(0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,1,1,1,1);
			n=rep(1,length(xx));
		},
		{	# 3
			titl1="No Overlap Ex.";
			xx=c(14,16,15,16.814,15.5,16.8058,14.9669,16.3314);
			yy=c(0,1,0,1,0,1,0,1);
			n=rep(1,length(xx));	
		},
		{	# 4
			titl1="Neyer Data, n=30";
			xx=c(60,70,65,68,64,60,60,68,61,67,62,70,62,69,
			63,69,63,63,69,71,70,70,62,62,70,62,70,61,60,58);
			yy=c(0,1,0,1,1,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0);
			n=rep(1,length(xx));	
		},
		{	# 5
			titl1="No ZMR, n=17";
			xx=c(23,17,14,12.5,9.75,11.13,12.57,11.85,12.21,14.61,13.41,12.63,11.19,11.91,12.66,12.4,12.3);
			yy=c(1,1,1,1,0,0,1,0,0,1,1,1,0,0,1,0,0);
			n=rep(1,length(xx));	
		},
		{	# 6
			titl1="Overlap, Infinite Sigma Case";
			# When the S >= 7, that's when Sigma=Infinity, and Response Prob is Constant (3/4)
			S=8;
			xx=c(10,12,11,S);
			yy=c(0,1,1,1);
			n=rep(1,length(xx));	
		},
		{	# 7
			titl1="Velocity, n=15";
			xx=c(656,900,950,984,1000,1022,1145,1164,1305,1313,1450,1457,1500,1625,1750)/1000;
			yy=c(0,0,0,0,0,0,1,0,1,1,1,1,0,1,1);
			n=c(1,1,4,1,1,1,1,1,1,1,3,1,1,1,1); 
			xx=rep(xx,n); yy=rep(yy,n);
			n=rep(1,length(xx));
		},
		{	# 8
			titl1="VariDensity, n=24";
			xx=c(1.8510,1.8505,1.8505,1.5530,1.6590,1.7580,1.7040,1.7560,1.8000,1.6590,1.7090,
			1.7570,1.8020,1.7040,1.7580,1.7266,1.7400,1.7472,1.7450,1.7540,1.8010,1.7241,1.7080,1.7311);
			yy=c(1,1,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,1,1,0,1);
			n=rep(1,length(xx));
		},
		{	# 9
			titl1="VariGap, n=21";
			xx=c(4975,6260,5850,5310,6080,5950,5775,5910,5740,5970,5890,5800,5730,5630,5420,5470,5500,5535,5495,5510,5610)/10000;
			yy=c(0,1,0,0,1,1,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0);
			n=rep(1,length(xx)); 
		},
		{	# 10
			titl1="NO ZMR Example"; 	
			xx=c(45,23,34,29,26,31.5,27.5,23.5,29.1,25,26.8,24.9,27.6,26.4,25.3,26.9,25.6,25.8,26.2);
			yy=c(1,0,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,0,0);
			n=rep(1,length(xx));	
		},
		{	# 11
			titl1="Dror & Steinberg (n=40)"; 
			xx=c(18.00,19.00,20.00,21.00,20.00,19.00,18.00,19.00,18.00,18.00,18.25,18.50,18.75,19.00,19.25,19.00,
			18.75,19.00,18.75,19.00,19.25,19.00,19.25,19.00,18.75,19.00,18.75,19.00,19.25,19.50,19.25,19.00,
			18.75,18.50,18.75,19.00,18.75,18.50,18.75,18.50);
			yy=c(0,0,0,1,1,1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,1,0,1,1,0,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0); 
			n=rep(1,length(xx));	
			#xx=c(18.0,18.25,18.5,18.75,18.75,19.0,19.0,19.25,19.25,19.5,20.0,20.0,21.0);
			#yy=c(0,0,0,0,1,0,1,0,1,1,0,1,1); n=c(4,1,4,8,1,6,7,1,4,1,1,1,1);
		},
		{	# 12
			titl1="Eli data (n=73)"; 
			xx=c(3.5,4.0,4.0,4.5,4.5,5.0,5.0,5.5,5.5,6.0);
			fac=1; xx=fac*xx;
			yy=c(0,0,1,0,1,0,1,0,1,1);
			n=c(5,23,1,6,5,2,6,3,7,15);
		},
		{	# 13
			titl1="A Neyer Test";
			xx=c(800,807,884,900,910,913,923,961,968,969,972,993,1000,
			1012,1013,1015,1025,1033,1038,1051,1060,1072,1129,1150,1219);
			yy=c(0,0,0,0,0,0,0,1,1,0,0,0,1,1,1,0,1,1,0,1,1,1,1,1,1);
			n=rep(1,length(xx));
		},
		{	#14
			titl1="JF's Data";
			xx=c(1.375,1.53,1.275,1.32,1.351,1.334,1.32,1.34,1.349,1.327,1.315,1.344,1.318,1.337,
			1.322,1.336,1.324,1.345,1.344,1.348,1.32,1.376,1.373,1.371,1.369,1.387,1.384,1.382,1.302,
			1.379,1.376,1.307,1.309,1.285,1.399,1.289)
			yy=c(1,1,0,0,1,1,0,0,1,1,0,1,0,1,0,0,0,0,0,0,1,1,1,1,0,1,1,1,0,0,1,1,0,0,1,1);
			n=rep(1,length(xx));
		},
		{ 	#15 
			titl1="An n=3 Ex."
			xx=c(1,3,4,5); yy=c(0,1,1,0); n=rep(1,length(xx));
			xx=c(1,3,4); yy=c(0,1,0); n=rep(1,length(xx));
		},
		{	#16
			titl1="An n=4, con=.5 Ex."
			xx=c(1,3,4,5); yy=c(0,1,1,0); n=rep(1,length(xx));
		},
		{	# 17
			titl1="(A) No overlap, n=3"; 
			xx=c(14,15,16); yy=c(0,1,1); tit1="No Overlap (n=3) Ex.";
			n=rep(1,length(xx));

		},
		{	# 18
			titl1="(B) No overlap, n=2"; 
			xx=c(14,16); yy=c(0,1); tit1="No Overlap (n=2) Ex.";
			n=rep(1,length(xx));

		},
		{	# 19
			titl1="(C) No overlap, n=4"; 
			xx=c(13,14,15,16); yy=c(0,0,1,1); tit1="No Overlap (n=4) Ex.";
			n=rep(1,length(xx));

		},
		{	# 20
			titl1="(A) One point overlap, n=2"; 
			xx=c(14,14); yy=c(0,1); #Need to do more testing
			n=rep(1,length(xx));
		},
		{	# 21
			titl1="(B) One point overlap, n=3"; 
			xx=c(14,16,16); yy=c(0,0,1); 
			n=rep(1,length(xx));
		},
		{	# 22
			titl1="(C) One point overlap, n=4"; 
			xx=c(14,16,16,16); yy=c(0,0,1,1); 
			n=rep(1,length(xx));
		},
		{	# 23
			titl1="(D) One point overlap, n=4"; 
			xx=c(14,16,16,18); yy=c(0,0,1,1); 
			n=rep(1,length(xx));
		},
		{	# 24
			titl1="One point overlap (E)"; 
			xx=c(14,16,16,16,16); yy=c(0,0,0,1,1); 
			n=rep(1,length(xx));
		},
		{ 	# 25
			titl1="A Simulated Test";
			xx=c(5.50000,16.50000,9.52628,7.23841,6.58790,10.46693,6.79797,10.14349,
			8.30392,6.94172,8.65883,7.03924,7.47790,6.81379,8.16104,8.86730,
			9.73525,9.57780,10.63076,10.45669,11.68522,23.80358,19.33242,17.14054,
			15.87435,15.04495,14.45109,17.11908,16.67776,16.31401,16.00650,15.74132,
			15.50899,15.30279,15.11780);
			yy=c(0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,0,1,0,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1);
			n=rep(1,length(xx));
		},
		{ 	# 26
			titl1="A 2 pointer, n=4 (overlap, Infinite Sigma)";
			xx=c(1,1,2,2);
			yy=c(0,1,1,0);
			n=rep(1,length(xx));
		},
		{ 	# 27
			titl1="A 2 pointer, n=6 (overlap)";
			xx=c(1,1,1,2,2,2);
			yy=c(0,0,1,0,1,1);
			n=rep(1,length(xx));
		}

);

titl1=paste(ic,ad,". ",titl1,sep="");
dat=matrix(c(xx,yy,n),ncol=3); 
dat=data.frame(dat);
names(dat)=c("X","Y","COUNT");

w=list(dat,titl1);
names(w)=c("d0","title");
g=lrmax(w,plt=plt0);
g=c(g,test=5,ttl0=titl1,ttl1=NULL,ttl2="wxdat",ttl3=NULL,ln=F)

return(g)
}

#INDEX 120 exes.R, (1 function)

otp=function(i)
{
if(!is.element(i,1:47)) cat(paste("otp function wants i between 1 and 47 \n\n",sep=""));

xWT <<- c(5.5,16.5,11,13.8,10.1,14.7,10.4,11.7,9.7,7.3,7.8,8.1,12.2,8.5,11.8);
yWT <<- c(0,1,0,1,0,1,1,1,1,0,0,0,1,0,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,0);
yNY <<- c(rep(0,5),1,rep(0,6),1,0,1,0,1,1,1,1);
ywLG1 <<- c(1,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0);

exi=list(
"w1 <<- gonogoSim(0,22,3,6,15,.9,1,plt=1,reso=.01,iseed=42983)",

"if(!exists(\"w1\")) w1 <<- gonogoSim(0,22,3,6,15,.9,1,plt=0,reso=.01,iseed=42983); w1",

"if(!exists(\"w1\")) w1 <<- gonogoSim(0,22,3,6,15,.9,1,plt=0,reso=.01,iseed=42983); 
yW=w1$d0$Y; w2 <<- gonogo(0,22,3,test=1,reso=.01,Y=yW)",

"if(exists(\"wWT\")) {wWT$ttl0 <<- \"Wu & Tian Table 1 Example\"; ptest(wWT,1);} else wWT <<- gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT)",

"if(!exists(\"wWT\")) wWT <<- gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT) else {wWT$title=\"3pod: Wu & Tian Table 1 Example\";
wWT$ttl0=\"Wu & Tian Table 1 Example\";wWT$units=\"in\"; wWT}",

"if(exists(\"wNY\"))wNY$d0 else wNY <<- gonogo(.6,1.4,.1,test=2,reso=.01,Y=yNY); wNY$d0;",

"if(exists(\"wNY\")){wNY$title=\"Neyer: 1994 Table 1 Example\";
wNY$ttl0=\"1994 Table 1 Example\";wNY$units=\"in\"; ptest(wNY,1)} else 
wNY <<- gonogo(.6,1.4,.1,test=2,reso=.01,Y=yNY);",

"if(exists(\"wb\")) ptest(wb,1) else wb <<- gonogoSim(10,10,.25,6,6,.9,1,plt=1,test=3,reso=.01,iseed=62517)",
"if(exists(\"ub\")) ptest(ub,1) else ub <<- gonogoSim(10,10,.25,6,6,.9,1,plt=1,test=3,reso=.01,BL=c(4,2,2),iseed=62517)",
"if(!exists(\"ub\")) ub <<- gonogoSim(10,10,.25,6,6,.9,1,test=3,reso=.01,BL=c(4,2,2),iseed=62517); pSdat1(ub,ud=T);",

"if(!exists(\"wLG2\")) wLG2 <<- gonogo(0,5,0,test=4,Y=ywLG1,X=2.5) else ptest(wLG2,1);",
"if(!exists(\"wLG3\")) wLG3 <<- gonogo(0,5,0,test=4,Y=ywLG1);  wLG3$d0; ",

"if(!exists(\"ny\")) ny <<- gonogoSim(.4,1.6,.1,20,test=2,plt=1,iseed=7865) else ptest(ny,1); abline(h=1+1.138/10,lty=2);
abline(h=1-1.138/10,lty=2);",

"if(!exists(\"wWT\")) wWT <<- gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT); wWT$jvec;",
"if(!exists(\"wWT\")) wWT <<- gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT); 
tbl=lims(1,wWT$d0,.95,Q=8.5); tbl;",
"if(!exists(\"wWT\")) wWT <<- gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT); 
tbl=lims(1,wWT$d0,.95,P=al15,Q=8.5); tbl;",
"blrb5();",

"if(!exists(\"wWT\"))wWT <<- gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT); ptest(wWT,3);
tbl=lims(1,wWT$d0,.95,P=al15,Q=8.5);
points(as.vector(tbl[c(7,9),c(1,3)]), rep(tbl[c(7,9),5],2),pch=16)",

"gonogo(0,22,3,reso=.0001,Y=yWT[-1])",

"w=gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT); 
m=cbind(data.frame(matrix(c(yWT,w$d0$X),ncol=2),w$d0$ID)); 
names(m)=c(\"Y\",\"X\",\"ID\"); m;",

"w=gonogo(0,220,30,reso=.001,Y=yWT,X=10*xWT); 
m=cbind(data.frame(matrix(c(yWT,w$d0$X),ncol=2),w$d0$ID)); 
names(m)=c(\"Y\",\"X\",\"ID\"); m;",

"w=gonogoSim(0,22,3,6,15,.9,1,reso=.0001,iseed=10);
m=cbind(data.frame(matrix(c(w$d0$Y,w$d0$X),ncol=2),w$d0$ID)); 
names(m)=c(\"Y\",\"X\",\"ID\"); m;",

"w=gonogoSim(0,22,3,6,15,.9,1,reso=.001,iseed=10,M=10);
m=cbind(data.frame(matrix(c(w$d0$Y,w$d0$X),ncol=2),w$d0$ID)); 
names(m)=c(\"Y\",\"X\",\"ID\"); m;",

"w=gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT); 
m=cbind(data.frame(matrix(c(yWT,w$d0$X),ncol=2),w$d0$ID)); 
names(m)=c(\"Y\",\"X\",\"ID\"); m;",

"w=gonogo(0,220,30,reso=.001,Y=yWT,X=10*xWT); 
m=cbind(data.frame(matrix(c(yWT,w$d0$X),ncol=2),w$d0$ID)); 
names(m)=c(\"Y\",\"X\",\"ID\"); m;",

"w=gonogoSim(0,22,3,6,15,.9,.8,reso=.0001,iseed=10);
m=cbind(data.frame(matrix(c(w$d0$Y,w$d0$X),ncol=2),w$d0$ID)); 
names(m)=c(\"Y\",\"X\",\"ID\"); m;",

"w=gonogoSim(0,22,3,6,15,.9,.8,reso=.001,iseed=10,M=10);
m=cbind(data.frame(matrix(c(w$d0$Y,w$d0$X),ncol=2),w$d0$ID)); 
names(m)=c(\"Y\",\"X\",\"ID\"); m;",

"if(!exists(\"wWTL\"))wWTL <<- gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT,ln=T); ptest(wWTL,1);",
"if(!exists(\"wWTL\"))wWTL <<- gonogo(0,22,3,reso=.0001,Y=yWT,X=xWT,ln=T); ptest(wWTL,3);",

"if(!exists(\"w1431\"))w1431 <<- gonogoSim(583.2,816.8,29.2,20,iseed=42983,plt=0); 
nmel3(70,2.92,.9,30,2000,83,icirc=1431)",

"if(!exists(\"un\"))un <<- gonogoSim(.6,1.4,.1,6,15,.9,1,plt=1,reso=.01,test=2,iseed=4257) else ptest(un,1)",
"if(!exists(\"un\"))un <<- gonogoSim(.6,1.4,.1,6,15,.9,1,plt=1,reso=.01,test=2,iseed=4257) else ptest(un,2)",
"if(!exists(\"un\"))un <<- gonogoSim(.6,1.4,.1,6,15,.9,1,plt=1,reso=.01,test=2,iseed=4257) else ptest(un,3)",
"if(!exists(\"un\"))un <<- gonogoSim(.6,1.4,.1,6,15,.9,1,plt=1,reso=.01,test=2,iseed=4257) else ptest(un,3)",
"if(!exists(\"un\"))un <<- gonogoSim(.6,1.4,.1,6,15,.9,1,plt=1,reso=.01,test=2,iseed=4257) else ptest(un,4)",

"if(!exists(\"ul\"))ul <<- gonogoSim(10,20,0,3,4,.5,1,plt=0,test=4,reso=.01,dm=-1,ds=.2,iseed=217); ptest(ul,5)",
"if(!exists(\"ul\"))ul <<- gonogoSim(10,20,0,3,4,.5,1,plt=0,test=4,reso=.01,,dm=-1,ds=.2,iseed=217); ptest(ul,6)",
"if(!exists(\"ul\"))ul <<- gonogoSim(10,20,0,3,4,.5,1,plt=0,test=4,reso=.01,,dm=-1,ds=.2,iseed=217); ptest(ul,7)",
"if(!exists(\"ul\"))ul <<- gonogoSim(10,20,0,3,4,.5,1,plt=0,test=4,reso=.01,,dm=-1,ds=.2,iseed=217); ptest(ul,8)",

"w6 <<- wxdat(6)",  "if(!exists(\"w6\"))w6 <<- wxdat(6,plt=F);  ptest(w6,5);", 
"w17 <<- wxdat(17)", "if(!exists(\"w17\"))w17 <<- wxdat(17,plt=F); ptest(w17,5);",
"w18 <<- wxdat(18)", "if(!exists(\"w18\"))w18 <<- wxdat(18,plt=F); ptest(w18,5);",
"w21 <<- wxdat(21)", "if(!exists(\"w21\"))w21 <<- wxdat(21,plt=F); ptest(w21,5);"

);

dt=exi[i];
eval(parse(text=dt));

}


