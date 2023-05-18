# sackade_lenght_distrubution
Introduction: The sackade_lenght_distrubution is a Python program that calculates the length distribution of Saccharomyces cerevisiae (baker's yeast) genome using a FASTA format file. The program uses Biopython package and can be used to analyze different genomes.

Installation:

Install Python3 on your system
Install Biopython package by running the following command on your terminal: pip install biopython
Clone the repository using the following command: git clone https://github.com/mendeltem/sackade_lenght_distrubution.git
Usage:

Navigate to the cloned directory and open a terminal.
Run the program using the following command: python3 length_distribution.py [path_to_fasta_file] For example: python3 length_distribution.py yeast_genome.fasta
The program will output a histogram plot of the length distribution of the genome.
Code structure: The program consists of one file named “length_distribution.py”. The file contains the following functions:

get_lengths(fasta_file): This function takes a FASTA format file as input and returns a list of sequence lengths in base pairs. This function uses Biopython package to read the sequences from the FASTA file.

plot_distribution(lengths): This function takes a list of sequence lengths as input and outputs a histogram plot of the length distribution of the genome. This function uses Matplotlib package to plot the histogram.

main(): This function is the main entry point of the program. It reads the input FASTA file, calculates the sequence lengths, and plots the length distribution of the genome.

Conclusion: The sackade_lenght_distrubution program is a simple yet useful tool for analyzing the length distribution of Saccharomy cerevisiae genome. The program can be easily customized to analyze other genomes as well by providing the corresponding FASTA format file.
