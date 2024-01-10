#simplifica otros obj mas complejos al formato empleado
NAME="botijo"
with open(f'../meshes/{NAME}.obj', 'r') as file:
    lines = file.readlines()

vertices = []
faces = []
vertList = []
faceList = []
print(lines[4:6])
for line in lines[4:]:
    if line.startswith('v ') or line.startswith('f ') or line.startswith('o '):
        if line.startswith('v '):
            vertices.append(line)
        elif line.startswith('f '):
            face = line.split()[1:]  # Split the line and extract the face vertices
            face_indices = [int(vertex.split('/')[0]) for vertex in face]  # Extract only vertex indices
            if len(face_indices) == 3:
                faces.append("f " + " ".join(map(str, face_indices)) + "\n")  # Reconstruct the face line
            elif len(face_indices) == 4:
                faces.append("f " + " ".join(map(str, face_indices[:3])) + "\n")  # Reconstruct the face line
                faces.append("f " + " ".join(map(str, face_indices[1:])) + "\n")  # Reconstruct the face line
        elif line.startswith('o '):
            vertList.append(vertices)
            faceList.append(faces)
            vertices = []
            faces = []
vertList.append(vertices)
faceList.append(faces)


for i in range(len(vertList)):
    print(i)
    with open(f'../meshes/simple/{NAME}{i}.obj', 'w') as file:
        file.write("# Converted OBJ\n")
        file.write("o ConvertedObject\n")
        # print(vertList)
        for vertex in [element for sublist in vertList[:(i+1)] for element in sublist] :
            file.write(''.join(vertex))

        for face in faceList[i]:
            file.write(''.join(face))
