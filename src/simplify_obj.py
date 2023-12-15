#simplifica otros obj mas complejos al formato empleado
with open('haskell.obj', 'r') as file:
    lines = file.readlines()

vertices = []
faces = []

for line in lines:
    if line.startswith('v ') or line.startswith('f '):
        if line.startswith('v '):
            vertices.append(line)
        elif line.startswith('f '):
            face = line.split()[1:]  # Split the line and extract the face vertices
            face_indices = [int(vertex.split('/')[0]) for vertex in face]  # Extract only vertex indices
            faces.append("f " + " ".join(map(str, face_indices)) + "\n")  # Reconstruct the face line

with open('converted_file.obj', 'w') as file:
    file.write("# Converted OBJ\n")
    file.write("o ConvertedObject\n")

    for vertex in vertices:
        file.write(vertex)

    for face in faces:
        file.write(face)
