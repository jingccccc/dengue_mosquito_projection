%calculate the warming period for 34 GCMs and 7 scenarios

clear all

mod={'ACCESS-CM2';'ACCESS-ESM1-5';'AWI-CM-1-1-MR';'BCC-CSM2-MR';'CAMS-CSM1-0';'CanESM5';'CAS-ESM2-0'; ...
    'CIESM';'CESM2';'CMCC-CM2-SR5';'CMCC-ESM2';'CNRM-CM6-1';'CNRM-ESM2-1';'E3SM-1-1'; ...
    'EC-Earth3';'FGOALS-g3';'FGOALS-f3-L';'FIO-ESM-2-0';'GFDL-ESM4';'GISS-E2-1-G';'HadGEM3-GC31-LL';'IITM-ESM'; ...
    'INM-CM5-0';'ISPL-CM6A-LR';'KACE-1-0-G';'MCM-UA-1-0';'MIROC6';'MIROC-ES2L'; ...
    'MPI-ESM1-2-LR';'MRI-ESM2-0';'NESM3';'NorESM2-LM';'TaiESM1';'UKESM1-0-LL'};

    

scen={'119';'126';'245';'370';'434';'460';'585'};
scen1={'SSP1-1.9';'SSP1-2.6';'SSP2-4.5';'SSP3-7.0';'SSP4-3.4';'SSP4-6.0';'SSP5-8.5'};

model_warming_time={};
turn=1;
for r=1:length(mod)
    tic
    model_warming_time(turn,1)=mod(r);
    list=dir(['F:\cmip6\' mod{r} '\tas_' '*' 'historical_r1i' '*.nc']);
    sdata=[];
    for j=1:length(list)%++++++++++++++++++++++
        mdata=ncread(['F:\cmip6\' mod{r} '\' list(j).name],'tas');
        sdata=cat(3,sdata,mdata);
    end
    if length(sdata)~=1980
        sdata(:,:,1981:end)=[];
    end
    predust=nanmean(nanmean(nanmean(sdata(:,:,1:612),1),2),3); 
    
    for s=1:length(scen)
        list=dir(['F:\cmip6\' mod{r} '\tas_' '*' scen{s} '_r1i' '*.nc']);
        if length(list)==0
            continue
        end
        model_warming_time(turn,2)=scen1(s);
        sdata=[];
        for j=1:length(list)%++++++++++++++++++++++
            mdata=ncread(['F:\cmip6\' mod{r} '\' list(j).name],'tas');
            sdata=cat(3,sdata,mdata);
        end
        if length(sdata)==1020
            sdata=cat(3,sdata,sdata(:,:,end-11:end));
        end
        
        projtas=[];
        for k=1:67
            projtas(:,:,k)=nanmean(nanmean(nanmean(sdata(:,:,k*12-11:k*12+228),1),2),3);%20年平均值
        end
        projtas=reshape(projtas,[],1);
        
        for warm=1:8
            aa=find(projtas>=predust+1+warm/2);
            if length(aa)>0&&aa(1)~=1 
                model_warming_time(turn,2+warm)={[num2str(aa(1)+2014) '-' num2str(aa(1)+2033)]};
            end
            if length(aa)>0&&projtas(1)-predust-1-warm/2<0.5&&aa(1)==1
                model_warming_time(turn,2+warm)={[num2str(aa(1)+2014) '-' num2str(aa(1)+2033)]};
            end
        end
        turn=turn+1;
    end
    toc
end
xlswrite('F:\cmip6\warming.xlsx',model_warming_time,1,'a3')
  
        
        