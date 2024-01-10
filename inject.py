def inject_content(file_a, file_b, mark='X', file_c='file_C.txt'):
    # Read content from file B
    with open(file_b, 'r') as b:
        content_b = b.read()

    # Read content from file A
    with open(file_a, 'r') as a:
        content_a = a.read()

    # Check if the mark exists in file A
    if mark in content_a:
        # Inject content B into file A at the mark
        updated_content = content_a.replace(mark, content_b)

        # Write the updated content to file C
        with open(file_c, 'w') as c:
            c.write(updated_content)
        print(f"Content from {file_b} injected into {file_a} at mark '{mark}'. File {file_c} created.")
    else:
        print(f"Mark '{mark}' not found in {file_a}. No changes made.")

# Usage example
inject_content('src/simulacionOrg.hs', 'src/objs.hs', mark='$%$%$%$%', file_c='src/simulacion.hs')