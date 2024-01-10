  (vertices1, triangles7) <- loadObjFile "../meshes/simple/palace7.obj"  
  (_, triangles6) <- loadObjFile "../meshes/simple/palace6.obj" 
  (_, triangles5) <- loadObjFile "../meshes/simple/palace5.obj" 
  (_, triangles4) <- loadObjFile "../meshes/simple/palace4.obj" 
  (_, triangles3) <- loadObjFile "../meshes/simple/palace3.obj" 
  (_, triangles2) <- loadObjFile "../meshes/simple/palace2.obj" 
  (_, triangles1) <- loadObjFile "../meshes/simple/palace1.obj" 
  (_, triangles0) <- loadObjFile "../meshes/simple/palace0.obj" 

  let !vertices1' = map (escalatePointt (4).movePoint (Direction 7.5 (-2.5) (-9.75)). rotatePointt 'Y' (287.5) ) vertices1
      !customTriangles7 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (vertices1', triangles7)
      !customTriangles6 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (vertices1', triangles6)
      !customTriangles5 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (vertices1', triangles5)
      !customTriangles4 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (vertices1', triangles4)
      !customTriangles3 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (vertices1', triangles3)
      !customTriangles2 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (vertices1', triangles2)
      !customTriangles1 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (vertices1', triangles1)
      !customTriangles0 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (vertices1', triangles0)
      !boundingVol7 = buildBVH 1000 customTriangles7
      !boundingVol6 = buildBVH 2000 customTriangles6
      !boundingVol5 = buildBVH 3000 customTriangles5
      !boundingVol4 = buildBVH 4000 customTriangles4
      !boundingVol3 = buildBVH 5000 customTriangles3
      !boundingVol2 = buildBVH 6000 customTriangles2
      !boundingVol1 = buildBVH 7000 customTriangles1
      !boundingVol0 = buildBVH 8000 customTriangles0

      figuras' =  Set.fromList $ addFigMult [(Acelerator boundingVol0),(Acelerator boundingVol1) ,(Acelerator boundingVol2),(Acelerator boundingVol3),(Acelerator boundingVol4),(Acelerator boundingVol5),(Acelerator boundingVol6),(Acelerator boundingVol7) ] $ Set.toList figuras

  let objFilePath2 = "../meshes/simplehaskell.obj"  
  (vertices2, triangles2) <- loadObjFile objFilePath2
  let vertices2' = map (escalatePointt (1).movePoint (Direction (-5) (-5) (-28)). rotatePointt 'Y' (90)) vertices2
      customTriangles2 = convertToCustomFormat (RGB 122 10 255) (0.85, 0,0) 0 (vertices2', triangles2)
      boundingVol'' = buildBVH 4000 customTriangles2
      figuras'' =  Set.fromList $ addFigMult [(Acelerator boundingVol'')] (Set.toList figuras')    
  