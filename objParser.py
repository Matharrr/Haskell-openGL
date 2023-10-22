# Cara pakai
# Buka blender dan buat objek 3D
# Klik kanan pada object tersebut dan ubahlah menjadi bentuk MESH
# export objek menjadi file obj
# Ubah lokasi file obj pada variabel berikut
obj_file_path = "pbp.obj"
# file output.txt akan muncul dan akan berisi source code haskell
# Copy kode tersebut ke dalam fungsi haskell

# Contoh hasil
# newDraw :: IO ()
# newDraw = do
#   let
#       v0 = Vertex3 (-1.0) (-1.0) (1.0)
#       v1 = Vertex3 (-1.0) (1.0) (1.0)
#       v2 = Vertex3 (-1.0) (-1.0) (-1.0)
#       v3 = Vertex3 (-1.0) (1.0) (-1.0)
#       v4 = Vertex3 (1.0) (-1.0) (1.0)
#       v5 = Vertex3 (1.0) (1.0) (1.0)
#       v6 = Vertex3 (1.0) (-1.0) (-1.0)
#       v7 = Vertex3 (1.0) (1.0) (-1.0)
#       n0 = Normal3 (-1.0) (-0.0) (-0.0)
#       n1 = Normal3 (-0.0) (-0.0) (-1.0)
#       n2 = Normal3 (1.0) (-0.0) (-0.0)
#       n3 = Normal3 (-0.0) (-0.0) (1.0)
#       n4 = Normal3 (-0.0) (-1.0) (-0.0)
#       n5 = Normal3 (-0.0) (1.0) (-0.0)
#   renderPrimitive Quads $ do
#       drawFace n0 v0 v1 v3 v2
#       drawFace n1 v2 v3 v7 v6
#       drawFace n2 v6 v7 v5 v4
#       drawFace n3 v4 v5 v1 v0
#       drawFace n4 v2 v6 v4 v0
#       drawFace n5 v7 v3 v1 v5


def read_obj_file(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        obj_data = file.read()
    return obj_data

def convert_obj_to_haskell_glut(obj_data):
    vertices = []
    normals = []
    faces = []
    listNormal = []
    numNormal = 0

    # Membaca data obj
    for line in obj_data.split('\n'):
        tokens = line.split()
        if tokens:
            if tokens[0] == 'v':
                x, y, z = map(float, tokens[1:4])
                vertices.append(f"Vertex3 ({x}) ({y}) ({z})")
            elif tokens[0] == 'vn':
                nx, ny, nz = map(float, tokens[1:4])
                normals.append(f"Normal3 ({nx}) ({ny}) ({nz})")
                listNormal.append(numNormal)
                numNormal += 1
            elif tokens[0] == 'f':
                face_indices = [list(map(int, val.split('/'))) for val in tokens[1:]]
                faces.append(face_indices)

    haskell_code = ""
    haskell_code += "  let\n"
    

    for i, vertex in enumerate(vertices):
        haskell_code += f"      v{i} = {vertex}\n"
    for i, normal in enumerate(normals):
        haskell_code += f"      n{i} = {normal}\n"
    
    haskell_code += "  renderPrimitive Polygon $ do\n"
    
    for i, face_indices in enumerate(faces):
            
        usedNormal = i
        if usedNormal not in listNormal:
            usedNormal = 0
        
        if len(face_indices) == 3:
            haskell_code += f"      drawPolygon n{usedNormal} "
            for vertex_index, _, normal_index in face_indices:
                vertex_var = f"v{vertex_index - 1}"
                haskell_code += f"{vertex_var} "
            haskell_code += "\n"
            
    haskell_code += "  renderPrimitive Quads $ do\n"
    
    for i, face_indices in enumerate(faces):
            
        usedNormal = i
        if usedNormal not in listNormal:
            usedNormal = 0
        
        if len(face_indices) == 4:
            haskell_code += f"      drawFace n{usedNormal} "
            for vertex_index, _, normal_index in face_indices:
                vertex_var = f"v{vertex_index - 1}"
                haskell_code += f"{vertex_var} "
            haskell_code += "\n"
        
    return haskell_code

obj_data = read_obj_file(obj_file_path)

haskell_glut_code = convert_obj_to_haskell_glut(obj_data)

with open("output.txt", "w") as output_file:
    output_file.write(haskell_glut_code)