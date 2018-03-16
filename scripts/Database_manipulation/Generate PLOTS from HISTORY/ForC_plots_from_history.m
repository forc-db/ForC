%ForC database
%code to generate ForC_plots from ForC_history and, optionally, re-number ForC_history (alphabetically by site, then plot).
%author: Kristina Anderson-Teixeira
%first published in Github public repository: Aug. 2017

%INPUT:
%ForC_history.xlsx-- this requires an .xlsx input file, so ForC_history.csv (master file)
    %must be saved as .xlsx in the same folder as this code. Before running this code,
    %it is essential to ensure that ForC_history.xlsx matches the master
    %version, so ForC_history.xlsx shoudl be deleted and re-saved before
    %each use os the code.

%OUTPUT:
%ForC_plots.mat--Matlab cell matrix. This must be pasted into
    %ForC_plots.xlsx for transformation to .csv format (see notes in that
    %file).
%histID.mat--Matlab matrix (double). This is optional output, which is
    %produced by  setting "renumber_history=1" (below).
    %To renumber/ reorder records, histID.mat must be pasted into
    %ForC_history.csv (history.ID field), and then the file sorted by that
    %field. If "renumber_history=1" is selected, ForC_plots must also be renumbered to match. 

%~~~~~~~~~~~~~~~~~~~~~~~

clear all; clf; close all;

%Here, select whether to renumber history (0-no; 1-yes)
renumber_history=0;  %CAUTION: ONLY SET TO 1 IF RENUMBERING HISTORYID!!!!

%input current master
[num text raw] = xlsread(strcat(pwd,'/ForC_history')); 

%read in all data fields
history.ID=raw(2:end, 1);
site=raw(2:end, 2);
    site_upper=upper(raw(2:end, 2)); %sites, transformed to all caps
plot=(raw(2:end, 3)); %plots,
    plot_upper=upper(raw(2:end, 3)); %plots, transformed to all caps 
plot_area=raw(2:end,4);
event_sequence=num(:,5); %sequence of events
date_=(raw(2:end,6)); %date (decimal year)
    date_num=num(:,6);
date.loc=raw(2:end,7);
    date.loc_num=num(:,7);
dist_cat=raw(2:end,8);
dist_type=raw(2:end,9);
est_regrowth_assumed_same_year=raw(2:end,10);
level=raw(2:end,11);
units=raw(2:end,12);
permort=raw(2:end,13);
distnotes=raw(2:end,14);
trop_extratrop=raw(2:end,16);

%create sequence variable
seq=event_sequence;
%seq=NaN*ones(length(site),1);
plotN=NaN*seq;

%create site-plot ID name, use it to identify unique plots, each of which gets one row in new version
site_plot=strcat(site_upper,'_',plot_upper);
plots_list=unique(site_plot);
n_plots=length(plots_list)

%define new variables/fields (caps):
    PLOTID=cell(n_plots,1);
    TROP_EXTRATROP=cell(n_plots,1);
    SITE=cell(n_plots,1);
    PLOT=cell(n_plots,1);
    PLOTAREA=cell(n_plots,1);    
    EST_RN=cell(n_plots,1);
    EST_DATE=cell(n_plots,1);
    EST_DATE_LOC=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    EST_DATE_INFERRED=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    EST_TYPE=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    EST_NOTES=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    REGROWTH_RN=cell(n_plots,1);
    REGROWTH_TYPE=cell(n_plots,1);
    REGROWTH_DATE=cell(n_plots,1);
    REGROWTH_DATE_LOC=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    REGROWTH_DATE_INFERRED=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    REGROWTH_PLANTING_DENSITY=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    REGROWTH_NOTES=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    DIST_MRS_RN=cell(n_plots,1);
    DIST_MRS_TYPE=cell(n_plots,1);
    DIST_MRS_DATE=cell(n_plots,1);
    MORT_MRS=cell(n_plots,1);
    DIST_MRS_ADD_RN=cell(n_plots,1);
    DIST_RN=cell(n_plots,3);
    DIST_DATE=cell(n_plots,2);
    DIST_TYPE=cell(n_plots,2);
    MORT=cell(n_plots,2);
    MANAGEMENT=cell(n_plots,1); %this field not to be included in ForC-db plots data file (as of Aug. 2017)
    CO2=cell(n_plots,1);
    TEMPERATURE=cell(n_plots,1);
    HYDRO=cell(n_plots,1);
    NUTRIENTS=cell(n_plots,1);
    BIOTA=cell(n_plots,1);
    OTHER=cell(n_plots,1);

    PRIOR_RN=cell(n_plots,1);
    

maxrecords=1;
%cycle through unique plots, pulling out data to assign to new variables.
for n=1:n_plots
    %index records for the plot
    index=find(strcmp(site_plot, plots_list(n))==1); %index for all records for the plot
        maxrecords=max(length(index),maxrecords); % set new maxrecords 
        if length(index)>10
            %disp(plots_list(n))
        end
    %seq(index)=linspace(1,length(index),length(index)); %assign sequence number according to current order listed
    plotN(index)=n;
    histID=plotN+seq./100;
        
    %fill in fields that are the same for all records in the original version
    index1=min(index); %index for just the first record for the plot 
    PLOTID(n,1)={floor(cell2mat(history.ID(index1)))};
    TROP_EXTRATROP(n,1)=trop_extratrop(index1);
    SITE(n,1)=site(index1);
    PLOT(n,1)=plot(index1);
    PLOTAREA(n,1)=plot_area(index1);
    
    %Indeces for HISTTYPE = no.info or no.disturbance. These will be used multiple times
    i_ni=find(strcmp(site_plot, plots_list(n))==1 & (strcmp(dist_cat,'No.info')==1));
    i_ND= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'No.disturbance')==1);
    
    if length(i_ni)>1
        disp(plots_list(n))
    end
    
    %establishment of oldest trees
    i1= find(strcmp(site_plot, plots_list(n))==1 & (strcmp(dist_type,'Establishment of oldest trees')==1));
    i2= find(strcmp(site_plot, plots_list(n))==1 & (strcmp(dist_type,'Establishment of key species')==1));
    i_est=[i1;i2];
    if length(i_est)==1
        EST_RN(n,1)={histID(i_est)};
        EST_DATE(n,1)=date_(i_est);
        EST_DATE_LOC(n,1)=date.loc(i_est);
        EST_DATE_INFERRED(n,1)=est_regrowth_assumed_same_year(i_est);  
        if isempty (i2)==1
            EST_TYPE(n,1)={'trees'};
        else
            EST_TYPE(n,1)={'key_taxa'};
        end
        EST_NOTES(n,1)=distnotes(i_est);
    elseif isempty(i_est)==1
        if isempty(i_ni)==1
            EST_RN(n,1)={0};
            EST_DATE(n,1)={'NAC'};
            EST_DATE_LOC(n,1)={'NA'};
            EST_DATE_INFERRED(n,1)={'NA'};
            EST_TYPE(n,1)={'NA'};
            EST_NOTES(n,1)={'NA'};
        elseif length(i_ni)==1
            EST_RN(n,1)={0};
            EST_DATE(n,1)=date_(i_ni);
            EST_DATE_LOC(n,1)={'NA'};
            EST_DATE_INFERRED(n,1)={'NA'};
            EST_TYPE(n,1)={'NA'};
            EST_NOTES(n,1)=distnotes(i_ni);
        end
    elseif length(i_est)>1
        disp (strcat('REVISIT BY HAND (ESTABLISHMENT): ',plots_list(n))) %revisit by hand
    end
    
    %regrowth (most recent)
    i1= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Regrowth')==1 & (strcmp(dist_type,'Initiation of post-disturbance cohort (natural)')==1));
    i3= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Regrowth')==1 & (strcmp(dist_type,'Initiation of post-disturbance cohort (planted or natural)')==1));
    i5= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Regrowth')==1 & (strcmp(dist_type,'Planted')==1));
    i6= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Regrowth')==1 & (strcmp(dist_type,'Seeded_trees')==1));
    i7= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Regrowth')==1 & (strcmp(dist_type,'Planted_and_natural_regeneration')==1));
    i_reg=[i1;i3;i5;i6;i7];
    if length(i_reg)==1
        REGROWTH_RN(n,1)={histID(i_reg)};
        REGROWTH_DATE(n,1)=date_(i_reg);
        REGROWTH_DATE_LOC(n,1)=date.loc(i_reg);
        EST_DATE_INFERRED(n,1)=est_regrowth_assumed_same_year(i_reg);  
        if isempty(i1)==0
            REGROWTH_TYPE(n,1)={'Initiation of post-disturbance cohort (natural)'}; 
            REGROWTH_PLANTING_DENSITY(n,1)={'NA'};
        elseif isempty(i3)==0
            REGROWTH_TYPE(n,1)={'Initiation of post-disturbance cohort (planted or natural)'};
            REGROWTH_PLANTING_DENSITY(n,1)={'NA'};
        elseif isempty(i5)==0
            REGROWTH_TYPE(n,1)={'Planted'};
            REGROWTH_PLANTING_DENSITY(n,1)=level(i5);
        elseif isempty(i6)==0
            REGROWTH_TYPE(n,1)={'Seeded_trees'};
            REGROWTH_PLANTING_DENSITY(n,1)=level(i6);
        elseif isempty(i7)==0
            REGROWTH_TYPE(n,1)={'Planted_and_natural_regeneration'};
            REGROWTH_PLANTING_DENSITY(n,1)=level(i7);
        end
        REGROWTH_NOTES(n,1)=distnotes(i_reg);
        seqREGROWTH(n,1)=seq(i_reg);
    elseif isempty(i_reg)==1
        if length(i_ni)==1  %cases with No.Info records. For these,we can know the missing value code
            REGROWTH_DATE(n,1)=date_(i_ni);
            REGROWTH_TYPE(n,1)=dist_type(i_ni);
            REGROWTH_NOTES(n,1)=distnotes(i_ni);
        else  %cases with no confirmed lack of recent regrowth but no regrowth info and no No.Info record to tell us why the info is missing. For these, we must assume 'NAC'.
            REGROWTH_DATE(n,1)={'NAC'};
            REGROWTH_TYPE(n,1)={'NAC'};
            REGROWTH_NOTES(n,1)={'NA'};
        end
        REGROWTH_RN(n,1)={0};
        REGROWTH_DATE_LOC(n,1)={'NA'};
        REGROWTH_DATE_INFERRED(n,1)={'NA'};
        REGROWTH_PLANTING_DENSITY(n,1)={'NA'};
        seqREGROWTH(n,1)=0;
    elseif length(i_reg)>1
        disp (strcat('REVISIT BY HAND (REGROWTH): ',plots_list(n))) %revisit by hand
    end

    
    %disturbance prior to regrowth (if any)

    i1= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Disturbance')==1 & seq<seqREGROWTH(n));

    i_mrsd=i1;

    if length(i_mrsd)==1 %cases with one record of disturbance prior to regrowth
        DIST_MRS_RN(n,1)={histID(i_mrsd)};
        DIST_MRS_DATE(n,1)=date_(i_mrsd);
        DIST_MRS_TYPE(n,1)=dist_type(i_mrsd);
        DIST_MRS_ADD_RN(n,1)={0};
        MORT_MRS(n,1)=permort(i_mrsd);
    elseif isempty(i_mrsd)==1  % cases with no records of disturbance prior to regrowth
         DIST_MRS_RN(n,1)={0};
         DIST_MRS_ADD_RN(n,1)={0};
         if length(i_ND)==1 %cases with confirmed lack of disturbance since a given date.
            DIST_MRS_DATE(n,1)=date_(i_ND);
            DIST_MRS_TYPE(n,1)=dist_type(i_ND);
            MORT_MRS(n,1)={'NA'};
        elseif length(i_ni)==1  %cases with No.Info records. For these,we can know the missing value code
            DIST_MRS_DATE(n,1)=date_(i_ni);
            DIST_MRS_TYPE(n,1)=dist_type(i_ni);
            MORT_MRS(n,1)=permort(i_ni);
        else  %cases with no confirmed lack of recent regrowth but no regrowth info and no No.Info record to tell us why the info is missing. For these, we must assume 'NAC'.
            DIST_MRS_DATE(n,1)={'NAC'};
            DIST_MRS_TYPE(n,1)={'NAC'};
            MORT_MRS(n,1)={'NAC'};
         end
    elseif length(i_mrsd)>1  % cases with multiple records of disturbance prior to regrowth
        i_mrsd2=max(i_mrsd);
        i_mrsd3=min(i_mrsd);
        DIST_MRS_RN(n,1)={histID(i_mrsd2)};
        DIST_MRS_DATE(n,1)=date_(i_mrsd2);
        DIST_MRS_TYPE(n,1)=dist_type(i_mrsd2);
        MORT_MRS(n,1)=permort(i_mrsd2);
        if length(i_mrsd)==2
            DIST_MRS_ADD_RN(n,1)={histID(i_mrsd3)};
        elseif length(i_mrsd)>2
            DIST_MRS_ADD_RN(n,1)={plotN(i_mrsd3)};
        end
    end
    
    %prior disturbance/ regrowth
    i1= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Regrowth_prior')==1);
    i2= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Disturbance_prior')==1);
    i_prior=[i1;i2];
    if length(i_prior)==1
        PRIOR_RN(n,1)={histID(i_prior)};
    elseif isempty(i_prior)==1
        PRIOR_RN(n,1)={0};
    elseif length(i_prior)>1
        PRIOR_RN(n,1)={plotN(i_prior(1))};
    end
    
    %disturbance after regrowth or in primary forests or forests with unknown disturbance history

    i1= find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Disturbance')==1 & seq>seqREGROWTH(n));
    
    i_dist=[i1];

    if isempty(i_dist)==0 
        for e=1:2
            if e<=length(i_dist)
                iX=i_dist(e);
                DIST_RN(n,e)={histID(iX)};
                DIST_DATE(n,e)=date_(iX);
                DIST_TYPE(n,e)=dist_type(iX);
                MORT(n,e)=permort(iX);
            else
                DIST_RN(n,e)={0};
                DIST_DATE(n,e)={'NA'};
                DIST_TYPE(n,e)={'NA'};  
                MORT(n,e)={'NA'}; 
            end
        end 
        for e=3
            if length(i_dist)==3
                DIST_RN(n,3)={histID(max(i_dist))};
            elseif length(i_dist)>3
                DIST_RN(n,3)={plotN(i_dist(1))};
            elseif length(i_dist)<3
                DIST_RN(n,3)={0};
            end
        end

    elseif isempty(i_dist)==1
        DIST_RN(n,:)={0};
        DIST_DATE(n,:)={'NA'};
        DIST_TYPE(n,:)={'NA'}; 
        MORT(n,:)={'NA'}; 
    end
    
   %managements  
    
    %set MANAGEMENT field to UM when there's a No.Disturbance record
    if ~isempty(i_ND)
        MANAGEMENT(n)= {'UM'};
    elseif isempty(i_ND)==1
        MANAGEMENT(n)= {''};
    end
    
    %CO2
    iCO2=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'CO2_fumigation')==1));
    if length(iCO2)==1
        CO2(n)= {histID(iCO2)};
    elseif isempty(iCO2)==1
        CO2(n)= {0};
    elseif length(iCO2)>1
        CO2(n)={plotN(iCO2(1))};
    end
    
    %temperature 
    i1=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Warming_soil')==1));
    i2=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Warming_air')==1));
    iTEMP= [i1; i2]; 
    if length(iTEMP)==1
        TEMPERATURE(n)= {histID(iTEMP)};
    elseif isempty(iTEMP)==1
        TEMPERATURE(n)= {0};
    elseif length(iTEMP)>1
        TEMPERATURE(n)={plotN(iTEMP(1))};
    end
    
    %hydrology
    i1=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Drained')==1));
    i2=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Irrigation')==1));
    i3=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Precipitation diversion')==1));
    iHYDRO= [i1; i2; i3]; 
    if length(iHYDRO)==1
        HYDRO(n)= {histID(iHYDRO)};
    elseif isempty(iHYDRO)==1
        HYDRO(n)= {0};
    elseif length(iHYDRO)>1
        HYDRO(n)={plotN(iHYDRO(1))};
    end
    
    %nutrients
    i1=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & ~contains(dist_type,'Fertilization')==0);
    iNUT= [i1]; 
    if length(iNUT)==1
        NUTRIENTS(n)= {histID(iNUT)};
    elseif isempty(iNUT)==1
        NUTRIENTS(n)= {0};
    elseif length(iNUT)>1
        NUTRIENTS(n)={plotN(iNUT(1))};
    end
    
    %biota
    i1=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Herbicide')==1));
    i2=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Pesticide')==1));
    i3=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Planted')==1));
    i4=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Seeded_agriculture')==1));
    iBIOTA= [i1; i2; i3; i4]; 
    if length(iBIOTA)==1
        BIOTA(n)= {histID(iBIOTA)};
    elseif isempty(iBIOTA)==1
        BIOTA(n)= {0};
    elseif length(iBIOTA)>1
        BIOTA(n)={plotN(iBIOTA(1))};
    end

    %other
    i1=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Soil disturbance')==1));
    i2=find(strcmp(site_plot, plots_list(n))==1 & strcmp(dist_cat, 'Management')==1 & (strcmp(dist_type,'Other')==1));
    iOTHER= [i1;i2]; 
    if length(iOTHER)==1
        OTHER(n)= {histID(iOTHER)};
    elseif isempty(iOTHER)==1
        OTHER(n)= {0};
    elseif length(iOTHER)>1
        OTHER(n)={plotN(iOTHER(1))};
    end
    
end

   %finish filling in MANAGEMENT status 
        %MANAGEMENT has already been set to 'UM' (unmanaged) for all sites with no.disturbance.    
    management_sum=cell2mat(CO2)+ cell2mat(TEMPERATURE) +cell2mat(HYDRO)+ cell2mat(NUTRIENTS) +cell2mat(BIOTA) +cell2mat(OTHER); %indicates whether there are any management records (non-zero values).
    MANAGEMENT(management_sum>0)={'M'}; %set to 'M' (managed) if there are any management events (based on management_sum). 
    MANAGEMENT(find(isempty(MANAGEMENT))==1)={'NAC'};  %plots with no management records but no confirmation of no.disturbance.


%create matrices grouping fields for output
OUT_ESTABLISHMENT=[EST_RN EST_DATE]; %establishment of oldest trees
OUT_REGROWTH=[REGROWTH_RN REGROWTH_TYPE REGROWTH_DATE ];  %regrowth
OUT_DIST_PREV=[DIST_MRS_RN DIST_MRS_TYPE MORT_MRS DIST_MRS_DATE DIST_MRS_ADD_RN]; % disturbance prior to most recent regrowth
OUT_DIST=[DIST_RN(:,1) DIST_TYPE(:,1) MORT(:,1) DIST_DATE(:,1) DIST_RN(:,2) DIST_TYPE(:,2) MORT(:,2) DIST_DATE(:,2) DIST_RN(:,3) ];  %disturbance after regrowth
OUT_MAN=[CO2 TEMPERATURE HYDRO NUTRIENTS BIOTA OTHER]; %management

if renumber_history==0
    ForC_plots=[PLOTID SITE PLOT PLOTAREA OUT_ESTABLISHMENT OUT_REGROWTH OUT_DIST_PREV PRIOR_RN OUT_DIST OUT_MAN TROP_EXTRATROP];
    save ForC_plots
    disp('Updated ForC_plots matrix (ForC_plots.mat) has been produced based on ForC_history.xlsx (this folder).')
    disp('WARNING! Ensure that ForC_history.xlsx is up to date (i.e., saved from current master ForC_history.csv) before using ForC_plots.mat to update ForC_plots.csv.' )
elseif renumber_history==1
    N=num2cell(linspace(1,n_plots, n_plots)');
    ForC_plots=[N SITE PLOT PLOTAREA OUT_ESTABLISHMENT OUT_REGROWTH OUT_DIST_PREV PRIOR_RN OUT_DIST OUT_MAN TROP_EXTRATROP];
    save ForC_plots histID
    disp('Updated ForC_plots matrix (ForC_plots.mat) and renumbered history.ID field (histID.mat) have been produced based on ForC_history.xlsx (this folder).')
    disp('WARNING! history.ID and corresponding ForC_plots entries have been renumbered! DO NOT update ForC_plots (ForC_plots.mat) without also updating the HistoryID field in ForC_history (histID.mat).')
    disp('WARNING! Ensure that ForC_history.xlsx is up to date (i.e., saved from current master ForC_history.csv) before using ForC_plots.mat to update ForC_plots.csv.' )
end

disp('DONE!')
