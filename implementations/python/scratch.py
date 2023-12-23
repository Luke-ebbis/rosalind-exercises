#! /usr/bin/env python3
"""A simple pipeline for variant annotation.

author: Sibbe Bakker

usage: sibbe_annotation.py [-h]
                           reference_genome sample_A read1_A read2_A sample_B
                           read1_B read2_B
positional arguments:
  reference_genome  Path to the reference genome that must be analysed.
  sample_A          Name of sample A.
  read1_A           The first read as a fastq file to beanalysed.
  read2_A           The second read as a fastq file to beanalysed.
  sample_B          Name of sample B.
  read1_B           The first read as a fastq file to beanalysed.
  read2_B           The second read as a fastq file to beanalysed.

optional arguments:
  -h, --help        show this help message and exit
"""
from sys import argv
import subprocess
import re
import os.path
import argparse


def index_reference(path_to_reference: str):
    """Making a index of a reference genome using bwa.

    :path_to_reference: str: The path to the reference genome that must be indexed.
    :return: tuple:paths to index files, each item in the tuple is a string.

    Description --- Using bwa index, the indices are made.
    """
    assert os.path.exists(path_to_reference), \
        FileNotFoundError(f'Path to reference genome {path_to_reference} ' \
                          'does not exist.')

    files_produced = (f'{path_to_reference}.amb',
                      f'{path_to_reference}.ann',
                      f'{path_to_reference}.bwt',
                      f'{path_to_reference}.pac',
                      f'{path_to_reference}.sa')
    file_exist = [os.path.exists(index_f) for index_f in files_produced]
    if not all(file_exist):
        command = f'bwa index {path_to_reference}'
        subprocess.run(command, shell=True, check=True)

    return files_produced


def map_dna_to_reference(path_to_reads: tuple, path_to_reference: str,
                         # clean:bool=False,
                         out_put_file: str = 'out.sam'):
    """Mapping paired fastq reads from DNA to a reference genome using bwa.

    :param path_to_reads: tuple:paths to the reads to be aligned. Each item in
     the tuple is a string of a path leading to the fastq file.
    :param path_to_reference: str: string of the path to the reference genome
     fasta file.
     :param clean: bool: A flag, when set to True, the index files are
      removed after. not implemented
     :param out_put_file: str: name of the resulting alignment file. out.sam
      by default. if the file exist, the file is overwritten.
    """
    read_exist = [os.path.exists(path) for path in path_to_reads]

    if not os.path.exists(out_put_file):
        assert all(read_exist), \
            FileNotFoundError('Check whether the supplied fastq files exist.')
        read1, read2 = path_to_reads

        indices = index_reference(path_to_reference)
        command = f"bwa mem {path_to_reference} {read1} {read2} > {out_put_file}"

        try:
            subprocess.run(command, shell=True, check=True)
        except:
            raise ChildProcessError(f"{command} cannot be run.")
    return out_put_file


def sam_to_bam(sam_file_path: str) -> str:
    """Sam to bam conversion using samtools view.

    :params sam_file_path: str: path to the sam file to be converted to a bam
     file.
     :return: str: the path to the bam file. Name can
    """
    assert os.path.exists(sam_file_path), \
        FileNotFoundError(f'Check whether the supplied sam file' \
                          f'{sam_file_path} exists.')
    rootname = re.findall(r'(.+)\.', sam_file_path)[0]
    output_file_path = f'{rootname}.bam'
    if not os.path.exists(output_file_path):
        command_bam = f'samtools view -b -o {rootname}.bam -S {rootname}.sam'
        command_sorted_bam = f'samtools sort -o {rootname}.bam ' \
                             f'-S {rootname}.sorted.bam'
        subprocess.run(command_bam, shell=True, check=True)
    return output_file_path


def bam_sorter(bam_file_path: str) -> str:
    """Sam to bam conversion using samtools view.

    :params bam_file_path: str: path to the bam file to be converted to a bam
     file.
     :return: str: the path to the sorted bam file. Name can
    """
    assert os.path.exists(bam_file_path), \
        FileNotFoundError(f'Check whether the supplied sam file' \
                          f'{bam_file_path} exists.')

    rootname = re.findall(r'(.+)\.', bam_file_path)[0]
    output_file_path = f'{rootname}.sorted.bam'
    if not os.path.exists(output_file_path):
        rootname = re.findall(r'(.+)\.', bam_file_path)[0]

        command_sorted_bam = f'samtools sort {rootname}.bam ' \
                             f'-o {rootname}.sorted.bam'
        subprocess.run(command_sorted_bam, shell=True, check=True)

    return output_file_path


def varient_calling(sam_file_path: str, path_to_reference: str,
                    max_depth: int = 80000,
                    outfile=None) -> str:
    """Varient calling with bcftools mpilup and call.

    :param sam_path: str: Path to the sam file of the mapping.
    :param reference_genome_path: str: Path to the reference genome with which
     `sam_path` was made.
    :param max_depth: int: The maximum depth value the mpileup tool will use
     to call SNPs.
    :param outfile: str: Path of the outfile to be written, defaults to
     `None`, meaning that a outfile is created in the format
      <sam_file_name>_<reference_file_name>.vcf. This file is saved in the
      same directory as `sam_file_path`.
     :return: str: Path to a vcf file holding the snps against the specified
      reference genome file.

      dependencies --- For this function to work, samtools and bcftools must
       be installed and availible with the $PATH variable.
    """
    if outfile == None:
        # Group 1 directory, 2 is slash, 3 is filename with extension.
        vcf_dir = re.findall('^(.+)\/([^\/]+)$', sam_file_path)[0][0]
        sam_root = re.findall(r'(\w+)\.', sam_file_path)[0]
        reference_root = re.findall(r'(\w+)\.', path_to_reference)[0]
        outfile = vcf_dir + f'/{sam_root}_{reference_root}.vcf'
    assert os.path.exists(path_to_reference), \
        FileNotFoundError(f'Path to reference genome {path_to_reference} ' \
                          'does not exist.')
    if not os.path.exists(outfile):
        bam_file_path = sam_to_bam(sam_file_path)
        sorted_bam_file_path = bam_sorter(bam_file_path)

        variant_likelyhood_command = 'bcftools mpileup ' \
                                     + f"-d {max_depth} -f" \
                                       f" {path_to_reference}" \
                                       f" {sorted_bam_file_path}"
        variant_call_command = 'bcftools call ' + \
                               f"-mv -Ov -o {outfile}"

        calling_pipe = variant_likelyhood_command \
                       + ' | ' + variant_call_command
        subprocess.run(calling_pipe, shell=True, check=True)
    return outfile


def compress_index_vcf(vcf_file_path: str) -> str:
    """Compressing a vcf file with bgzip and make an index.

    :param vcf_file_path: str: Path to the target vcf file.
    :return: str: path of the compressed vcf file.
    """
    assert os.path.exists(vcf_file_path), \
        FileNotFoundError(f'Path to vcf file {vcf_file_path} ' \
                          'does not exist.')
    outfile_path = vcf_file_path + '.gz'
    if not os.path.exists(outfile_path):
        subprocess.run(f'bgzip -c {vcf_file_path} > {outfile_path}',
                       shell=True, check=True)
        subprocess.run(f'bcftools index {outfile_path}',
                       shell=True, check=True)
    return outfile_path


def merge_vcf_files(vcf_file_paths: tuple, outfile='merged.vcf') -> str:
    """Merging vcf files to a single file

    :param vcf_file_paths: tuple: A tuple of vcf_file_paths. Each item in the
     tuple is a string of the path to the VCF_file.
    :param outfile: str: Path of the outfile to be written, defaults to
     `None`, meaning that a outfile is created in the format
      <sample_names>.vcf. This file is saved in the
      same directory as `vcf_file_path`. Not yet implemented.
    :return: str: the merged vcf file path.
    """
    file_exists = [os.path.exists(vcf_file_path) \
                   for vcf_file_path in vcf_file_paths]
    assert all(file_exists), \
        FileNotFoundError(f'Path to vcf file {vcf_file_paths} ' \
                          'does not exist.')

    if outfile == 'merged.vcf':
        folder = re.compile('^(.+)\/([^\/]+)$')

        # Group 1 directory, 2 is slash, 3 is filename with extension.
        vcf_dir = folder.findall(vcf_file_paths[0])[0][0]
        outfile_path = f'{vcf_dir}/{outfile}'
    else:
        outfile_path = outfile
    if not os.path.exists(outfile):
        # for the joining, vcf files must be compressed, I will compress them:
        compressed_vcfs = [compress_index_vcf(vcf_file) for \
                           vcf_file in vcf_file_paths]
        files_to_merge = ' '.join(compressed_vcfs)
        command = f'bcftools merge {files_to_merge} -Ov -o {outfile_path}'
        subprocess.run(command, shell=True, check=True)
    return outfile_path


def filter_vcf_on_region(vcf_file_path: str, region: str,
                         outfile=None) -> str:
    """Filtering a vcf file based on a genomic region using tabix.

    :param vcf_file_path: str: Path to the target vcf file.
    :param region: str: a string indicating region to be included, example
     chr1:57646425-57803821.
    :param outfile: str: Path of the outfile to be written, defaults to
     `None`, meaning that a outfile is created in the format
      <vcf_file_path>_<start>-<end>.vcf. This file is saved in the
      same directory as `vcf_file_path`.
    :return: str: the filtered vcf file path.

    implementation --- Not fully implemented.
    """
    assert os.path.exists(vcf_file_path), \
        FileNotFoundError(f'Path to vcf file {vcf_file_path} ' \
                          f'does not exist.')
    if outfile == None:
        folder = re.compile('^(.+)\/([^\/]+)$')
        name__parts = folder.findall(vcf_file_path)[0]
        outfile_path = f'{name__parts[0]}/{region}' \
                       f'_{name__parts[1]}'

    if not os.path.exists(outfile_path):
        compressed_vcf = compress_index_vcf(vcf_file_path)
        command = f'tabix -f -p vcf {compressed_vcf} {region}'
        print(command)
        subprocess.run(command, shell=True, check=True)
    return outfile_path


def main():
    # Getting options and arguments
    desc = """Comparing SNPs between two samples\n

    This python script uses samtools bcftools. To call SNPs from two
    populations of sample and place the calls into a single file called
    merged.vcf.\n

    Example of usage      ./sibbe_annotation.py yeast/chr3.fasta CENPK  yeast/cenpk-chr3_1.fastq yeast/cenpk-chr3_2.fastq IMW004 yeast/imw004-chr3_1.fastq yeast/imw004-chr3_2.fastq

    Coming implementation: filter on a region of interest in a gff file.
    """
    parser = argparse.ArgumentParser(description=desc)

    parser.add_argument("reference_genome", type=str,
                        help="Path to the reference genome that must be " \
                             "analysed.")

    parser.add_argument("sample_A", type=str,
                        help="Name of sample A.")
    parser.add_argument("read1_A", type=str,
                        help="The first read as a fastq file to be " \
                             "analysed.")
    parser.add_argument("read2_A", type=str,
                        help="The second read as a fastq file to be " \
                             "analysed.")

    parser.add_argument("sample_B", type=str,
                        help="Name of sample B.")
    parser.add_argument("read1_B", type=str,
                        help="The first read as a fastq file to be " \
                             "analysed.")
    parser.add_argument("read2_B", type=str,
                        help="The second read as a fastq file to be " \
                             "analysed.")

    args = parser.parse_args()

    path_to_reference = args.reference_genome

    readsA = (args.read1_A, args.read2_A)  # CENPK
    readsB = (args.read1_B, args.read2_B)  # imw004

    name_a = args.sample_A
    name_b = args.sample_B

    region_of_interest = 'yeast/ADY2.gff'

    # print(f'Analysing sample A: {name_a}\nAnalysing sample B: {name_b}')

    # Runnng the alignment
    mapA = map_dna_to_reference(readsA,
                                path_to_reference,
                                out_put_file=f"results/{name_a}.sam")
    mapB = map_dna_to_reference(readsA,
                                path_to_reference,
                                out_put_file=f"results/{name_b}.sam")
    # Calling the SNPs
    snps_A = varient_calling(sam_file_path=mapA,
                             path_to_reference=path_to_reference)
    snps_B = varient_calling(sam_file_path=mapB,
                             path_to_reference=path_to_reference)
    vcfs = (snps_A, snps_B,)

    # Investigating a single gene
    merged_vcf = merge_vcf_files(vcfs)
    # filtered_vcf = filter_vcf_on_region(merged_vcf, region_of_interest)


if __name__ == "__main__":
    main()
