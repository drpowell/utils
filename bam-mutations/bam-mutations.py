#!/usr/bin/env python

import sys
import pysam

BAM_CMATCH=0
BAM_CINS=1
BAM_CDEL=2
BAM_CREF_SKIP=3
BAM_CSOFT_CLIP=4
BAM_CHARD_CLIP=5

def process_bam(file):
    infile = pysam.AlignmentFile(file, "r")
    tot_del=0
    tot_ins=0
    tot_mis=0
    tot_match=0
    tot_reads=0
    for s in infile:
        if s.is_unmapped or s.mapping_quality<5 or s.is_supplementary:
            continue
        ref_pos = s.reference_start
        s_pos = 0
        cigar = s.cigartuples
        seq = s.query_sequence
        n_del=0
        n_ins=0
        n_mis=0
        n_match=0
        tot_reads += 1
        for (t,n) in cigar:
            if t==BAM_CSOFT_CLIP or t==BAM_CHARD_CLIP:
                s_pos += n
            elif t==BAM_CINS:
                n_ins+=1
                s_pos += n
            elif t==BAM_CDEL:
                n_del+=1
                ref_pos += n
            elif t==BAM_CMATCH:
                for i in range(0,n):
                    if seq[s_pos+i]==ref_seq[ref_pos+i]:
                        n_match+=1
                    else:
                        n_mis+=1
                ref_pos += n
                s_pos += n
            else:
                raise ValueError("Bad CIGAR string %s"%t)
        #print(s.is_reverse, n_match,n_mis,n_ins,n_del)
        tot_match += n_match
        tot_mis += n_mis
        tot_ins += n_ins
        tot_del += n_del

    #print("\t".join(str(x) for x in (file,tot_match,tot_mis,tot_ins,tot_del)))
    print(file + "\t" + "\t".join(str(x/tot_reads) for x in (tot_match,tot_mis,tot_ins,tot_del)))

if len(sys.argv)<=1:
    print("Usage: %s <ref.fasta> [file1.bam] [file2.bam]"%sys.argv[0])
    sys.exit(1)

ref = pysam.FastxFile(sys.argv[1]).__next__()
ref_seq = ref.sequence
print("Loaded %s length %d"%(ref.name,len(ref_seq)))

print("\t".join(str(x) for x in ("file","match","mismatch","insert","delete")))
for f in sys.argv[2:]:
    process_bam(f)
