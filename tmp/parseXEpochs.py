import os
import re
import sys

def read_files(n, etapaY, etapasX, pix, piy, etapasY, maxN):
    filename = "a"
    regex_pattern = filename + str(n) + "_" + str(etapaY) + "_.*"
    matching_files = sorted([f for f in os.listdir('.') if re.match(regex_pattern, f)])
    matching_files.sort(key=lambda x: int(re.search(r'\d+', x.split('_')[2]).group()))
    concatenated_pixels = []
    piyxetapa = (piy // (etapasY * maxN))

    header = ""
    print(matching_files)
    for i in range(piyxetapa):
        for file in matching_files:
            with open(file, 'r') as f:
                content = f.readlines()
                header = content[:6]
                lines = content[6]  
                pixels = lines.split()  
                interval_size = (len(pixels)//piyxetapa)
                start = i * interval_size
                end = (i + 1) * interval_size 
                # print(len(pixels[start:end]))
                concatenated_pixels.extend(pixels[start:end]) 

    
    output_content = ''
    output_content += ' '.join(concatenated_pixels) + " \n"

    new_filename = f"{filename}{n}_{etapaY}.ppm"
    with open(new_filename, 'w') as f:
        f.write(''.join(header))
        f.write(output_content)

if __name__ == "__main__":
    if len(sys.argv) != 8:
        print("Usage: python script.py n etapaY etapasX pix piy etapasY maxN")
    else:
        n = int(sys.argv[1])
        etapaY = int(sys.argv[2])
        etapasX = int(sys.argv[3])
        pix = int(sys.argv[4])
        piy = int(sys.argv[5])
        etapasY = int(sys.argv[6])
        maxN = int(sys.argv[7])
        read_files(n, etapaY, etapasX, pix,piy,etapasY,maxN)