  (texturas1,vertices1, triangles9) <- loadObjFile "../meshes/simple/palace9.obj"
  (_,_, triangles8) <- loadObjFile "../meshes/simple/palace8.obj" 
  (_,_, triangles7) <- loadObjFile "../meshes/simple/palace7.obj"   
  (_,_, triangles6) <- loadObjFile "../meshes/simple/palace6.obj" 
  (_,_, triangles5) <- loadObjFile "../meshes/simple/palace5.obj" 
  (_,_, triangles4) <- loadObjFile "../meshes/simple/palace4.obj" 
  (_,_, triangles3) <- loadObjFile "../meshes/simple/palace3.obj" 
  (_,_, triangles2) <- loadObjFile "../meshes/simple/palace2.obj" 
  (_,_, triangles1) <- loadObjFile "../meshes/simple/palace1.obj" 
  (_,_, triangles0) <- loadObjFile "../meshes/simple/palace0.obj" 

  -- let !vertices1' = map (escalatePointt (2).movePoint (Direction (4) (-6) (7)). rotatePointt 'Y' (90)) vertices1
  let !vertices1' = map (escalatePointt (1). extraR.movePoint (Direction (6) (-5) (8)). rotatePointt 'Y' (90)) vertices1

      texturas1' = if length texturas1 == 0 then [Point2D 0 0] else texturas1
      !customTriangles9 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 10 Nada (vertices1', (map (head) triangles9), texturas1, (map (!! 1) triangles9 ))
      !customTriangles8 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 11 Nada(vertices1', (map (head) triangles8), texturas1, (map (!! 1) triangles8 ))
      !customTriangles7 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 12 Nada (vertices1', (map (head) triangles7), texturas1, (map (!! 1) triangles7 ))
      !customTriangles6 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 13 Nada (vertices1', (map (head) triangles6), texturas1, (map (!! 1) triangles6 ))
      !customTriangles5 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 14 Nada (vertices1', (map (head) triangles5), texturas1, (map (!! 1) triangles5 ))
      !customTriangles4 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 15 Nada (vertices1', (map (head) triangles4), texturas1, (map (!! 1) triangles4 ))
      !customTriangles3 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 16 Nada (vertices1', (map (head) triangles3), texturas1, (map (!! 1) triangles3 ))
      !customTriangles2 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 17 Nada (vertices1', (map (head) triangles2), texturas1, (map (!! 1) triangles2 ))
      !customTriangles1 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 18 Nada (vertices1', (map (head) triangles1), texturas1, (map (!! 1) triangles1 ))
      !customTriangles0 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0.05) 0 19 Nada (vertices1', (map (head) triangles0), texturas1, (map (!! 1) triangles0 ))
      !boundingVol9 = buildBVH 1000 customTriangles9
      !boundingVol8 = buildBVH 1000 customTriangles8
      !boundingVol7 = buildBVH 1000 customTriangles7
      !boundingVol6 = buildBVH 2000 customTriangles6
      !boundingVol5 = buildBVH 3000 customTriangles5
      !boundingVol4 = buildBVH 4000 customTriangles4
      !boundingVol3 = buildBVH 5000 customTriangles3
      !boundingVol2 = buildBVH 6000 customTriangles2
      !boundingVol1 = buildBVH 7000 customTriangles1
      !boundingVol0 = buildBVH 8000 customTriangles0

      figuras' =  Set.fromList $ addFigMult [(Acelerator boundingVol0),(Acelerator boundingVol1) ,(Acelerator boundingVol2),(Acelerator boundingVol3),(Acelerator boundingVol4),(Acelerator boundingVol5),(Acelerator boundingVol6),(Acelerator boundingVol7),(Acelerator boundingVol8),(Acelerator boundingVol9) ] $ Set.toList figuras

--   (verticesB1, trianglesB0) <- loadObjFile "../meshes/simple/botijo0.obj"  
  

--   let !verticesB1' = map (escalatePointt (4).movePoint (Direction 0 (-2) (0)). rotatePointt 'Y' (0)) verticesB1
--       !customTrianglesB0 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 (verticesB1', trianglesB0)
--       !boundingVolB0 = buildBVH 1000 customTrianglesB0
--       !customTrianglesB1 = convertToCustomFormat (RGB 220 120 50) (0.85, 0,0) 0 ((map (movePoint (Direction (-8) (0) (0))) verticesB1'), trianglesB0)
--       !boundingVolB1 = buildBVH 1000 customTrianglesB1
--       !customTrianglesB2= convertToCustomFormat (RGB 20 120 220) (0.85, 0,0) 0 ((map (movePoint (Direction (-2) (0) (3))) verticesB1'), trianglesB0)
--       !boundingVolB2= buildBVH 1000 customTrianglesB2
--       !customTrianglesB3 = convertToCustomFormat (RGB 120 220 50) (0.85, 0,0) 0 ((map (movePoint (Direction 4 (0) ((-3))))verticesB1'), trianglesB0)
--       !boundingVolB3 = buildBVH 1000 customTrianglesB3
--       !customTrianglesB4 = convertToCustomFormat (RGB 100 120 200) (0.85, 0,0) 0 ((map (movePoint (Direction 0 (5) (-8)))verticesB1'), trianglesB0)
--       !boundingVolB4 = buildBVH 1000 customTrianglesB4
--       !customTrianglesB5 = convertToCustomFormat (RGB 0 220 0) (0.85, 0,0) 0 ((map (movePoint (Direction 0 (0) (-15)))verticesB1'), trianglesB0)
--       !boundingVolB5 = buildBVH 1000 customTrianglesB5
--       figuras'' =  Set.fromList $ addFigMult [(Acelerator boundingVolB0),(Acelerator boundingVolB1),(Acelerator boundingVolB2),(Acelerator boundingVolB3),(Acelerator boundingVolB4),(Acelerator boundingVolB5)] $ Set.toList figuras'

  -- let objFilePath2 = "../meshes/simplehaskell.obj"  
  -- (vertices2, trianglesH2) <- loadObjFile objFilePath2
  -- let vertices2' = map (escalatePointt (2).movePoint (Direction (-5) (-5) (-8)). rotatePointt 'Y' (90)) vertices2
  --     customTrianglesH2 = convertToCustomFormat (RGB 122 10 255) (0.85, 0,0) 0 (vertices2', trianglesH2)
  --     boundingVol'' = buildBVH 4000 customTrianglesH2
  --     figuras''' =  Set.fromList $ addFigMult [Acelerator boundingVol''] (Set.toList figuras)    
  
  let objFilePath2 = "../meshes/haskell.obj"  
  (texturas2, vertices2, trianglesH2) <- loadObjFile objFilePath2
  let vertices2' = map (escalatePointt (0.5).extraR.movePoint (Direction (26) (-4) (-17)). rotatePointt 'Y' (180)) vertices2
      origtrianglesH2 = map (head) trianglesH2
      textTrianglesH2 = map (!! 1) trianglesH2
      -- texturas2' = if length texturas2 == 0 then [Point2D 0 0] else texturas2
      customTrianglesH2 = convertToCustomFormat (RGB 122 10 255) (0.85, 0,0) 0 (Set.size figuras') Nada (vertices2', origtrianglesH2, texturas2, textTrianglesH2)
      boundingVol'' = buildBVH 4000 customTrianglesH2
      figuras'' =  Set.fromList $ addFigMult [Acelerator boundingVol''] (Set.toList figuras')    
  
  let objFilePath3 = "../meshes/botijo2.obj"  
  (texturas3, verticesb, trianglesB) <- loadObjFile objFilePath3
  let origtrianglesB = map (head) trianglesB
      textTrianglesB = map (!! 1) trianglesB
      texturas3' = if length texturas3 == 0 then [Point2D 0 0] else texturas3
      verticesb' = map (escalatePointt (10).extraR.movePoint (Direction (0) (0) (0)). rotatePointt 'Y' (0)) verticesb
      customTrianglesB = convertToCustomFormat (RGB 134 89 45) (0.85, 0,0) 0 20 Nada (verticesb', origtrianglesB, texturas3', textTrianglesB)
      boundingVol'' = buildBVH 4000 customTrianglesB
    --   verticesb'' = map (escalatePointt (4).extraR.movePoint (Direction (-5) (-1) (-17)). rotatePointt 'Y' (0)) verticesb
    --   customTrianglesB = convertToCustomFormat (RGB 134 89 45) (0.85, 0,0) 0 (Set.size figuras'') Nada (verticesb'', origtrianglesB, texturas3, textTrianglesB)
    --   boundingVol''' = buildBVH 4000 customTrianglesB
    --   verticesb''' = map (escalatePointt (4).extraR.movePoint (Direction (-5) (-1) (-17)). rotatePointt 'Y' (0)) verticesb
    --   customTrianglesB = convertToCustomFormat (RGB 134 89 45) (0.85, 0,0) 0 (Set.size figuras'') Nada (verticesb''', origtrianglesB, texturas3, textTrianglesB)
    --   boundingVol'''' = buildBVH 4000 customTrianglesB
    --   verticesb'''' = map (escalatePointt (4).extraR.movePoint (Direction (-5) (-1) (-17)). rotatePointt 'Y' (0)) verticesb
    --   customTrianglesB = convertToCustomFormat (RGB 134 89 45) (0.85, 0,0) 0 (Set.size figuras'') Nada (verticesb'''', origtrianglesB, texturas3, textTrianglesB)
    --   boundingVol''''' = buildBVH 4000 customTrianglesB
      figuras''' =  Set.fromList $ addFigMult [(Acelerator boundingVol''){- ,(Acelerator boundingVol'''),(Acelerator boundingVol''''),(Acelerator boundingVol''''') -}] (Set.toList figuras'')    
  
  
  -- let objFilePath2 = "../meshes/duck.obj"  
  -- (texturas2, vertices2, trianglesH2) <- loadObjFile objFilePath2
  -- let vertices2' = map (escalatePointt (2).movePoint (Direction (0) (0) (0)). rotatePointt 'Y' (0)) vertices2
  --     origtrianglesH2 = map (head) trianglesH2
  --     textTrianglesH2 = map (!! 1) trianglesH2
  --     -- texturas2' = if length texturas2 == 0 then [Point2D 0 0] else texturas2
  --     customTrianglesH2 = convertToCustomFormat (RGB 122 10 255) (0.85, 0,0) 0 (Set.size figuras) (vertices2', origtrianglesH2, texturas2, textTrianglesH2)
  --     boundingVol'' = buildBVH 4000 customTrianglesH2
  --     figuras''' =  Set.fromList $ addFigMult [Acelerator boundingVol''] (Set.toList figuras)    
  