# Overview
This repo contains a quick and simple analysis of Tax Rate Structures in
the United States Tax Policy.  In prticular the analysis will attempt
to determine a flat tax percentage to impose on Americans income in order to
meet the individual income tax revenues that the U.S. has under the current
taxing policy as of 2015.  

## Motivation
The idea for doing this analysis came up while discussing politics and tax 
policies with a friend of mine.  Upon further research I have found that 
a 2016 Presidential Candidate, Dr. Ben Carson, has made a proposal of a 
flat income tax for Americans of somewhere in the neighborhood of 10-15%
(according to the economists that he has consulted with.)

## Data
I use a python script to scrape .xls files from an [IRS website](http://www.irs.gov/uac/SOI-Tax-Stats---Individual-Statistical-Tables-by-Size-of-Adjusted-Gross-Income#_grp1)
The data scraped consists of All U.S. Individual Tax Returns classified by
income size and accumulated size of adjusted gross income for the years 
1996 - 2012.  However, to get a quick and dirty rough extimate of a flat tax
rate, I will only be using the 2012 data for the time being.
