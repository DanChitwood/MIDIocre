# The evolution of Western tonality: a corpus analysis of 24,000 songs from 190 composers over six centuries

## Abstract

The corpus of Western music offers the chance to analyze trends in its evolution. Here, we analyze greater than 24,000 MIDI (Musical Instrument Digital Interface) transcriptions of Western classical music from 197 composers spanning from the 15th to 20th centuries. The unique file format of MIDI files (notes of discrete frequencies turning “on” and “off” at specific times) allows us to statistically quantify note usage with respect to pitch class and intervals. We first perform a Principal Component Analysis (PCA) on pitch class and show that the data creates a ring. Songs strongly associated with a particular key fall on the ring, whereas highly chromatic and atonal works fall in the center, revealing that the evolution of Western music is constrained by the circle of fifths. We then examine interval usage. Discriminant analysis on composer identity reveals that a major source of evolutionary change is incremental and linear. Interval usage predicts individual composer identity at levels above chance. A Self-Organizing Map (SOM) reveals four major groups of composers with similar tonal styles. These groups are loosely based on traditional musical eras, but reveal unexpected continuity between co-existing schools of composers.

## The manuscript

This repository is meant to supplement the manuscript "The evolution of Western tonality: a corpus analysis of 24,000 songs from 190 composers over six centuries", which has been posted as a pre-print on SocArXiv and can be found [here](doi.org/10.17605/OSF.IO/BTSHK) at the following doi: [10.17605/OSF.IO/BTSHK](doi.org/10.17605/OSF.IO/BTSHK)

## This repository

The repository contains all code and data to recreate figures and analyses in the manuscript

* id_information.txt: ID numbers and associated MIDI file names

* extracted_data.txt: pitch class and interval data, composer information

* figures: jpg files of figures used in the manuscript

* circle_of_fifths_analysis: R code and data to recreate figures 1-3 and associated analyses

* interval_anlaysis: R code and data to recreate figures 4-7 and associated analyses


