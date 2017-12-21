tic;
tstart=tic;
a=1;
b=1;
c=0;
l=0;
d=0;
q=0;
q1=0;
q2=0;
q3=0;
flag=0;
Ex=0;
Breaker_Number(:,2)=Brn(:,2);
Breaker_Number(:,3)=Brn(:,3);
for i=1:119 %this loop is to check if any breaker status within a substaion is changed, and if yes, it saves the breaker and corresponding substation number 
    if Brn(i,5)~=Breaker_Number(i,5)
        if Breaker_Number(i,4)==3
            Out(a,1)=Breaker_Number(i,8);
            Out(a,2)=Breaker_Number(i,1);
            a=a+1;
        end
    end
end
a=a-1;
for i=1:24 %this loop will make a matrix of subtations which have a status change in their breaker(s).
    for j=1:a
        if Out(j,1)==i
            Sub(1,b)=i;
            b=b+1;
            break
        end
    end
end
b=b-1;
for i=1:90
    quit=0;
    for int2=1:b
        if abs(Conf(i,2))==Sub(1,int2) || Conf(i,3)==Sub(1,int2)
           quit=1;
        end
    end
    if Conf(i,2)<0 && quit==1
        Conf(i,2)=-Conf(i,2);
    end
end
%%%%
for i=1:40
    max(1,i)=i;
end
Ex=exist('SB','var');
if Ex~=0
int=size(SB);
int=int(1,2);
for i=1:24
    quit=0;
    for j=1:int
       for int2=1:b
           if i==Sub(1,int2)
               quit=1;
           end
       end
       if SB(i,j)==0 || quit==1
           break
       end
       if SB(i,j)>24
           int1=SB(i,j);
           max(1,int1)=0;
       end
    end
end
end

%%%%
for i=1:b
    for j=1:119
        if Breaker_Number(j,8)==Sub(1,i)&& Breaker_Number(j,4)==3
            Breaker_Number(j,2)=Sub(1,i); Breaker_Number(j,3)=Sub(1,i);
        end        
    end
    for j=1:90
        if Conf(j,11)==Sub(1,i)
            Conf(j,2)=Conf(j,11);
        end
        if Conf(j,12)==Sub(1,i)
            Conf(j,3)=Conf(j,12);
        end
    end
end
for i=1:b
    for cn=1:119
        if Breaker_Number(cn,5)==0 && Breaker_Number(cn,8)==Sub(1,i) && Breaker_Number(cn,4)==3
            a=a+1;
            Out(a,1)=Sub(1,i);Out(a,2)=cn;
        end
    end
end
S=119;
Br(1,1,2)=1000; %this is to avoid dimensions error further on
for p=1:b %this the start of what is called module A
    flag=0;
    p1=p+1; 
    d=0;
    List(1,1,p1)=0; %avoid dimensions error
for k=1:a
    q=size(Br);
    q1=q(1,1);q2=q(1,2);q3=q(1,3);q=0;
    for t=1:q3
        for y=1:q1
            for u=1:q2
                if Br(q1,q2,q3)==Out(k,2) || Breaker_Number(Out(k,2),5)~=0 
                    q=1;
                    break
                end
                if Breaker_Number(Out(k,2),5)==0 && Breaker_Number(Out(k,2),8)==Sub(1,p)
                    flag=1;
                end
                if q==1,break,end
            end
            if q==1,break,end
        end
    end
    q1=0;
    pt=size(List);
    p1=pt(1);p2=pt(2);p3=pt(3);p3=p3-1;
    for m=1:p3
    for j=1:p1
    for i=1:p2
            if List(j,i,m)==Breaker_Number(Out(k,2),6) && m==p;
                q1=1;
            end
            if List(j,i,m)==Breaker_Number(Out(k,2),7) && m==p;
                c=1;
            end
    end
    end
    end
    f=1;
    if q==0
    if Breaker_Number(Out(k,2),8)==Sub(1,p)
        if q1==0
            d=d+1;
        List(d,f,p)=Breaker_Number(Out(k,2),6);
        Br(d,f,p)=Out(k,2);
        i=1;while i<=f
            if List(d,f,p)<0 
            for j=1:S
            if Breaker_Number(j,4)==3&&Breaker_Number(j,8)==Sub(1,p)&&Breaker_Number(j,5)==1
                if Breaker_Number(j,6)==List(d,i,p)
                    for w=1:f
                        if Breaker_Number(j,7)==List(d,w,p)
                            l=1;
                        end
                    end
                    while l==0    
                    f=f+1;
                    List(d,f,p)=Breaker_Number(j,7);
                    Br(d,f,p)=j;
                    break
                    end
                    l=0;
                end
                if Breaker_Number(j,7)==List(d,i,p)
                    for w=1:f
                        if Breaker_Number(j,6)==List(d,w,p)
                            l=1;
                        end
                    end
                    while l==0
                   f=f+1;
                   List(d,f,p)=Breaker_Number(j,6);
                   Br(d,f,p)=j;
                   break
                    end
                    l=0;
                end
            end
            end
            end
        i=i+1;
        end
        end
        for i=1:f
            if List(d,i,p)==Breaker_Number(Out(k,2),7);
                c=1;
            end
        end
        if c==0
            d=d+1;
            f=1;
            List(d,f,p)=Breaker_Number(Out(k,2),7);
        Br(d,f,p)=Out(k,2);
        if Breaker_Number(Out(k,2),7)<0
        i=1;while i<=f
            for j=1:119
            if Breaker_Number(j,4)==3 && Breaker_Number(j,8)==Sub(1,p)
                if j~=Out(k,2)&&Breaker_Number(j,5)==1
                if Breaker_Number(j,6)==List(d,i,p)
                    for w=1:f
                        if Breaker_Number(j,7)==List(d,w,p)
                            l=1;
                        end
                    end
                    while l==0
                    f=f+1;
                    List(d,f,p)=Breaker_Number(j,7);
                    Br(d,f,p)=j;
                    break
                    end
                    l=0;
                end
                if Breaker_Number(j,7)==List(d,i,p)
                    for w=1:f
                        if Breaker_Number(j,6)==List(d,w,p)
                            l=1;
                        end
                    end
                    while l==0
                    f=f+1;
                    List(d,f,p)=Breaker_Number(j,6);
                   Br(d,f,k)=j;
                   break
                    end
                    l=0;
                end
                end
            end
            end
            i=i+1;
        end
        end        
        end
        c=0;
    end
end
end
if flag==0
    gx=1;
    for zx=1:119
        if Breaker_Number(zx,8)==Sub(1,p) && Breaker_Number(zx,4)==3
            E=size(List);
            E=E(1,2);
            l1=0;
            l2=0;
            for w1=1:E
                if List(1,w1,p)==Breaker_Number(zx,6)
                    l1=1;
                end
                if List(1,w1,p)==Breaker_Number(zx,7)
                    l2=1;
                end
            end
            if l1~=1    
                List(1,gx,p)=Breaker_Number(zx,6);
                gx=gx+1;
            end
            if l2~=1
                List(1,gx,p)=Breaker_Number(zx,7);
                gx=gx+1;
            end
        end
    end
end   
end
%%%%%%%%%this is the beginning of the extension
Ex=exist('List','var');
if Ex~=0
si=size(List);
in1=si(2);
in2=0;
counter=0;
E1=size(List);
d=E1(1,1);
for i=1:b
    in=0;
    hmatrix(i,1)=Sub(1,i);
    for j=1:d
        if List(j,1,i)~=0
            in=in+1;          
        end
    end
    hmatrix(i,2)=in;
    for k=1:in        
        for j=1:in1
        if List(k,j,i)~=0        
        hmatrix(i,k+in+j+counter+1)=abs(List(k,j,i));
        in2=in2+1;
        end
        end        
        hmatrix(i,k+2)=in2;
        counter=counter+in2-1;
        in2=0;
    end
    counter=0;
end
end
%%%%%%%%end of build help matrix
di=1;
for i=1:119 %this loop is to check if any breaker status within a substaion is changed, and if yes, it saves the breaker and corresponding substation number 
    if Brn(i,5)~=Breaker_Number(i,5)
        if Breaker_Number(i,4)==2
            Dis(1,di)=Breaker_Number(i,6);
            Dis(2,di)=Breaker_Number(i,5);
            di=di+1;
        end
    end
end
di=di-1;
Data=Breaker_Number;
Con=Conf;
for i=1:di
    op=0;
    op1=Dis(1,i);
    if Dis(2,i)==0 && Con(op1,2)>0
        Con(op1,2)=-Con(op1,2);
    end
    if Dis(2,i)==1
        for j=1:119
            if Breaker_Number(j,6)==op1 && Breaker_Number(j,5)==1
                op=op+1;
            end
        end
        if op==2 && Con(op1,2)<0
            Con(op1,2)=-Con(op1,2);
        end
    end
end
max1=24;
Ex=exist('hmatrix','var');
if Ex~=0
for i=1:b
    in=hmatrix(i,2);
    counter=0;
    for j=1:in
        in1=0;
        if hmatrix(i,j+2)==1
            for m=1:j
                in1=in1+hmatrix(i,m+2);
            end
            in2=hmatrix(i,in+in1+2);
            if in2<66
                    Con(in2,2)=-Con(in2,2);                
            else
                if Con(in2,3)==0%%This is to check for shunt availability
                    Con(in2,2)=-Con(in2,2);
                else
                    max1=max1+1;
                    while max(1,max1)==0
                        max1=max1+1;
                    end
                    Con(in2,2)=max(1,max1);
                    for br=1:119
                        if abs(Breaker_Number(br,6))==Con(in2,1) && Breaker_Number(br,8)==Con(in2,11)
                            Breaker_Number(br,2)=max(1,max1);
                        end
                        if abs(Breaker_Number(br,7))==Con(in2,1) && Breaker_Number(br,8)==Con(in2,11)
                            Breaker_Number(br,3)=max(1,max1);
                        end
                    end
                end
            end
        else
            counter=counter+1;
            if counter>1
            for m=1:j-1
                in1=in1+hmatrix(i,m+2);
            end
            max1=max1+1;
            while max(1,max1)==0
                max1=max1+1;
            end
            in3=hmatrix(i,j+2);
            for q=1:in3
                in2=hmatrix(i,in+in1+q+2);                
                if hmatrix(i,1)==Con(in2,2)                
                    Con(in2,2)=max(1,max1);
                    for br=1:119
                        if abs(Breaker_Number(br,6))==Con(in2,1) && Breaker_Number(br,8)==Con(in2,11)
                            Breaker_Number(br,2)=max(1,max1);
                        end
                        if abs(Breaker_Number(br,7))==Con(in2,1) && Breaker_Number(br,8)==Con(in2,11)
                            Breaker_Number(br,3)=max(1,max1);
                        end
                    end                    
                    if Con(in2,4)~=0
                        Con(in2,4)=max(1,max1);
                    end
                else 
                    Con(in2,3)=max(1,max1);
                    for br=1:119
                        if abs(Breaker_Number(br,6))==Con(in2,1) && Breaker_Number(br,8)==Con(in2,12)
                            Breaker_Number(br,2)=max(1,max1);
                        end
                        if abs(Breaker_Number(br,7))==Con(in2,1) && Breaker_Number(br,8)==Con(in2,12)
                            Breaker_Number(br,3)=max(1,max1);
                        end
                    end                    
                end                
            end                            
        end
    end
    end 
end
end
for i=1:66
    if Con(i,7)==0 && Con(i,2)>0
        Con(i,2)=-Con(i,2);
    end
    if Con(i,5)==0 && Con(i,6)==0 && Con(i,2)>0
        Con(i,2)=-Con(i,2);
        Con(i,7)=0;
    end
end

%%%%%%end of step 1 changes
d=0;
f=0;
List1=0;
for i=1:max1
    flag=0;
    S1=size(List1);
    S2=S1(1,2);
    S1=S1(1,1);
    for j=1:S1
        for k=1:S2
            if List1(j,k)==i
                flag=1;
            end
        end
    end
    if flag==0
        f=1;
        d=d+1;
        List1(d,f)=i;
        t=1;
        while t<=f
            for p=1:66
                flag1=0;
                if Con(p,2)==List1(d,t)
                    S1=size(List1);
                    S2=S1(1,2);
                    S1=S1(1,1);
                    for j=1:S1
                        for k=1:S2
                            if List1(j,k)==Con(p,3)
                                flag1=1;
                            end
                        end
                    end
                    if flag1==0
                        f=f+1;
                        List1(d,f)=Con(p,3);
                    end
                end
                if Con(p,3)==List1(d,t)
                    S1=size(List1);
                    S2=S1(1,2);
                    S1=S1(1,1);
                    for j=1:S1
                        for k=1:S2
                            if List1(j,k)==Con(p,2)
                                flag1=1;
                            end
                        end
                    end
                    if flag1==0 && Con(p,2)>0
                        f=f+1;
                        List1(d,f)=Con(p,2);
                    end
                end
            end
            t=t+1;
        end
    end
end
%%%The end(Still some little works)
n=size(List1);
m=n(1,1);
n=n(1,2);
for z=1:66
    for z1=1:m
        for z2=1:n
            if abs(Con(z,2))==List1(z1,z2)
                Con(z,9)=z1;
            end
            if abs(Con(z,3))==List1(z1,z2)
                Con(z,10)=z1;
            end
        end
    end
end
%%%%%%%%%
for z=1:66
    if Con(z,9)~=Con(z,10)
        if Con(z,5)==0 && Con(z,6)==0 && Con(z,2)>0
            Con(z,2)=-Con(z,2);
        end
    end
end
SB=0;
for i=1:24
    co=1;
    SB(i,1)=i;
    for j=1:119
        sign1=0; sign2=0;
        if Breaker_Number(j,2)~=Breaker_Number(j,3) && Breaker_Number(j,8)==i && Breaker_Number(j,4)==3
            if Breaker_Number(j,2)>24
               for k=1:co
                if SB(i,k)==Breaker_Number(j,2)
                    sign1=1;
                end
               end
               if sign1==0
                   co=co+1;
                   SB(i,co)=Breaker_Number(j,2);
               end
            end            
            if Breaker_Number(j,3)>24
               for k=1:co
                if SB(i,k)==Breaker_Number(j,3)
                    sign2=1;
                end
               end
               if sign2==0
                   co=co+1;
                   SB(i,co)=Breaker_Number(j,3);
               end
            end
        end
    end
end
for i=1:66
    if Con(i,9)==Con(i,10)
        Con(i,7)=1;
    end
end
Sl=size(List1);
Sl1=Sl(1,2);
Sl=Sl(1,1);
Sl2=Sl1+1;
for i=1:Sl
    flagd=0;
    for j=1:Sl1
        for k=1:119
            if Breaker_Number(k,4)==1 && Breaker_Number(k,5)==1 && Breaker_Number(k,2)==List1(i,j)
                flagd=1;
                break
            end
            if flagd==1
            break
            end
        end
        if flagd==1
        break
        end
    end
    if flagd==1
        List1(i,Sl2)=5;
    else
        List1(i,Sl2)=6;
    end
end

Brn=Breaker_Number;
Conf=Con;
tel=toc(tstart);
            
                

