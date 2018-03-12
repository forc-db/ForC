%Code to group sites into areas based on geographical proximity
%Written by K. Anderson-Teixeira on 12/06/17

close all; clear all;  clc;

%READ IN SITE DATA:
[num text raw] = xlsread('ForC_sites.xlsx');

%pull out latitude-longitude
coords=num(:,6:7);

%define areas 
area=clusterdata(coords, .75);

n_sites=length(num)
n_areas=length(unique(area))


