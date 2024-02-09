%calculate for two type of mosquitoes
clear all

load('F:\obslonlat')%grids of Asian

mos1=xlsread('F:\aegypti.csv',1,'f2:h22138');
a=find(mos1(:,3)>=1979);
mos1=mos1(a,:);
mos1=[mos1(:,2) mos1(:,1) mos1(:,3)];
a=mos1(:,1:2);
a=(round(a*2+0.5))/2-0.25;
mos1(:,1:2)=a;

b=[];
for i=1:length(mos1)
    a=find(lon_obs(:,1)==mos1(i,1)&lat_obs(:,1)==mos1(i,2));
    if isempty(a)
        b=[b;i];
    end
end 
mos1(b,:)=[];


mos2=xlsread('F:\albopictus.csv',1,'f2:h22138');
a=find(mos2(:,3)>=1979);
mos2=mos2(a,:);
mos2=[mos2(:,2) mos2(:,1) mos2(:,3)];
a=mos2(:,1:2);
a=(round(a*2+0.5))/2-0.25;
mos2(:,1:2)=a;
b=[];
for i=1:length(mos2)
    a=find(lon_obs(:,1)==mos2(i,1)&lat_obs(:,1)==mos2(i,2));
    if isempty(a)
        b=[b;i];
    end
end 
mos2(b,:)=[];


mo1=zeros(36,12404);
mo2=zeros(36,12404);
for i=1979:2014
    a1=find(mos1(:,3)==i);
    a2=find(mos2(:,3)==i);
    for j=1:length(a1)
        a=find(lon_obs==mos1(a1(j),1)&lat_obs==mos1(a1(j),2));
        mo1(i-1978,a)=mo1(i-1978,a)+1;
    end
    for j=1:length(a2)
        a=find(lon_obs==mos2(a2(j),1)&lat_obs==mos2(a2(j),2));
        mo2(i-1978,a)=mo2(i-1978,a)+1;
    end
end