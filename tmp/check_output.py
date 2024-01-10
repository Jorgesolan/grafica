def write_corrected_max_ppm(filename, dimensions, maxrgb, pixels):
    with open(filename, 'w') as f:
        f.write("P3\n")
        f.write(' '.join(dimensions)+"\n")
        f.write(str(maxrgb)+"\n")
        f.write(' '.join(pixels) + '\n')

def write_clamped_max_ppm(filename, dimensions, rgb, fixedpixels):
    with open(filename, 'w') as f:
        f.write("P3\n")
        f.write(' '.join(dimensions)+"\n")
        f.write(str(rgb)+"\n")
        f.write(' '.join(fixedpixels) + '\n')

def write_filled_ppm(filename, dimensions, rgb, fixedpixels):
    with open(filename, 'w') as f:
        f.write("P3\n")
        f.write(' '.join(dimensions)+"\n")
        f.write(str(rgb)+"\n")
        f.write(' '.join(fixedpixels) + '\n')

def check_ppm_data(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()

        # Extract image dimensions from the header
        dimensions = lines[3].strip().split(' ')
        width = int(dimensions[0])
        height = int(dimensions[1])

        # Extract color depth from the header
        color_depth = int(lines[4])

        # Calculate expected number of pixels based on dimensions
        expected_pixels = width * height

        # Check if the number of data points matches the expected count
        pixel_data = lines[6].split()
        actual_pixels = len(pixel_data) // 3  # Assuming RGB format, hence 3 values per pixel

        # Validate color depth
        valid_color_depth = all(0 <= int(pixel) <= color_depth for pixel in pixel_data)

        # Check for data mismatch
        if expected_pixels != actual_pixels:
            print(f"Error: Data mismatch - Expected {expected_pixels} pixels, found {actual_pixels} pixels.")
            filledpixels = pixel_data + ['0' for i in range (expected_pixels-actual_pixels)]
            write_filled_ppm("output_filled.ppm",dimensions,color_depth,filledpixels)
        elif not valid_color_depth:
            print(f"Error: Color depth mismatch - Color values exceed specified depth of {color_depth}.")
            real_max = max([pixel for pixel in pixel_data if not 0 <= int(pixel) <= color_depth])
            clampedpixels = [pixel if (0 <= int(pixel) <= color_depth) else str(color_depth) for pixel in pixel_data]
            write_corrected_max_ppm("output_fixed_max.ppm",dimensions,real_max,pixel_data)
            write_clamped_max_ppm("output_clamped.ppm",dimensions,color_depth,clampedpixels)
        else:
            print("Data check passed. No mismatch found.")

check_ppm_data('output.ppm')