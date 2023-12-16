import os
import re
import sys

def read_files(n, etapaY, etapasX, pix):
    filename = "a"
    regex_pattern = filename + str(n) + "_" + str(etapaY) + "_.*"
    matching_files = sorted([f for f in os.listdir('.') if re.match(regex_pattern, f)])
    matching_files.sort(key=lambda x: int(re.search(r'\d+', x.split('_')[2]).group()))
    concatenated_pixels = []
    interval_size = (pix // etapasX) * 3
    header = ""
    print(matching_files)
    for i in range(etapasX):
        for file in matching_files:
            with open(file, 'r') as f:
                content = f.readlines()
                header = content[:6]
                # print(len(content))
                lines = content[6]  # Skip the first 4 lines
                pixels = lines.split()  # Get pixels from the first line
                start = i * interval_size
                end = (i + 1) * interval_size 
                concatenated_pixels.extend(pixels[start:end])  # Concatenate pixels

    
    output_content = ''
    output_content += ' '.join(concatenated_pixels)

    # Write the new content to a file
    new_filename = f"{filename}{n}_{etapaY}.ppm"
    with open(new_filename, 'w') as f:
        f.write(''.join(header))
        f.write(output_content)

if __name__ == "__main__":
    if len(sys.argv) != 5:
        print("Usage: python script.py n etapaY etapasX pix")
    else:
        n = int(sys.argv[1])
        etapaY = int(sys.argv[2])
        etapasX = int(sys.argv[3])
        pix = int(sys.argv[4])
        read_files(n, etapaY, etapasX, pix)