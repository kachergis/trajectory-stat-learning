#!/usr/bin/env python
'''
combine subject files into one data file
'''

import sys, os
DATA_DIR = "data"

def build_line(line=[]):
    string = ""
    for i in line:
        string += str(i)+'\t'
    return (string.strip('\t') + '\n')

def main():
	fname = "exp2_saffran_traj_data.txt"
	testfname = "exp2_saffran_traj_test_data.txt"
	try:
		f = open(fname, 'r')
		f.close()
		print(fname + " already exists! Let's not overwrite it.")
		sys.exit()
	except:
		pass
	
	files = os.listdir(os.path.join(os.curdir, DATA_DIR))
	outfile = open(fname, 'w')
	outfile_t = open(testfname, 'w')
	# prevTarget / nextTarget at test are blank (sometimes) -- add a -1 
	header = 'subject\ttrial\tcond\ttotalTime\ttrialTime\tprevTarget\tcurTarget\tnextTarget\thitTargetPos\ttimeTargetHit\tobjTouched\tCorrect\txPos\tyPos\tscore\n'
	outfile.write(header)
	outfile_t.write(header)
	ncols = len(header.split())
	for file in files:
		if file[0:4]=="data":
			sub_file = open(os.path.join(os.curdir, DATA_DIR, file), 'r')
			for line in sub_file:
				#Ttrial = line.strip().split()
				sp = line.find(' ')
				if sp==-1: #len(Ttrial)>2 and Ttrial[2]=="test": #(or if no spaces were found)
					tp = line.find('\t\t\t\t')
					if tp==-1: # not the extra-short line
						tp = line.find('\t\t')
						tmp = line[:tp] + '\tNA\t' + line[tp+1:]
					else:
						tmp = line[:tp] + '\tNA\tNA\t\NA\t' + line[tp+1:]

					Ttrial = tmp.strip().split()
					try:
						int(Ttrial[0])
						outfile_t.write(build_line(Ttrial))	
					except:
						pass
				elif sp != -1: #if there is a space, line is not a test trial
					tmp = line[:sp] + line[sp+1:] # remove the space in the condition column (e.g., 'Misaligned Que'='MisalignedQue')
					trial = tmp.strip().split()
					try:
						int(trial[0])
						#if file[6:8]=='21': # special case for screwed up subj number
						#	trial[0] = '21'

						outfile.write(build_line(trial))
					except:
						pass	
	outfile.close()
	outfile_t.close()

if __name__ == '__main__':
	main()
