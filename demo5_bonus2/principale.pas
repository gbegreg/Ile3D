unit principale;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Viewport3D, System.Math.Vectors, FMX.Controls3D, FMX.Objects3D,
  FMX.MaterialSources, FMX.types3D, FMX.Effects, System.UIConsts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Ani, System.Threading,
  Math, system.RTLConsts, FMX.Layouts;

type
  TWaveRec = record
    P, W, origine : TPoint3D;
    function Wave(aSum, aX, aY, aT : single):Single;
  end;
  TfPrincipale = class(TForm)
    v3D: TViewport3D;
    dmyMonde: TDummy;
    mSol: TMesh;
    dmyJoueurOrientation: TDummy;
    dmyJoueur: TDummy;
    Camera1: TCamera;
    faniPrincipale: TFloatAnimation;
    dmyProchainePosition: TDummy;
    ColorMaterialSource2: TColorMaterialSource;
    ColorMaterialSource1: TColorMaterialSource;
    Layout1: TLayout;
    TrackBar1: TTrackBar;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    LightMaterialSource2: TLightMaterialSource;
    pMer: TPlane;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure v3DMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure v3DMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure faniPrincipaleProcess(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure v3DPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure mSolRender(Sender: TObject; Context: TContext3D);
    procedure TrackBar1Change(Sender: TObject);
    procedure pMerRender(Sender: TObject; Context: TContext3D);
  private
    { Déclarations privées }
    FPosDepartCurseur: TPointF;    // Position du pointeur de souris au début du mouvement de la souris
    procedure SetAngleDeVue(const Value: TPointF); // Modification de l'angle de vue
    function GetDirection: TPoint3D;
    function Barycentre(p1, p2, p3: TPoint3D; p4: TPointF): single;
    function CalculerHauteur(P: TPoint3D): single;
    procedure interactionIHM;
    procedure CalcMesh(aPlane: TPlane; origine, P, W: TPoint3D; maxMesh: integer);

  public
    { Déclarations publiques }
    maHeightMap: TBitmap;    // Texture qui servira à générer le sol (le Mesh)
    moitieCarte, fps : integer;
    demiHauteurJoueur, miseAEchelle, demiHauteurSol, temps, vitesse : single;
    Center : TPoint3D;
    procedure ChargerTextures;
    procedure CreerIle(const nbSubdivisions: integer);
    property posDepartCurseur: TPointF read FPosDepartCurseur write FPosDepartCurseur; // Propriété de la position du pointeur de souris au début du mouvement de la souris
    property angleDeVue : TPointF write SetAngleDeVue; // Propriété de l'angle de vue
    property direction : TPoint3D read GetDirection; // Propriété de la direction
  end;

  TMeshHelper = class(TCustomMesh); // Va servir pour caster un TPlane en TMesh

const
  MaxSolMesh = 255;  /// Nombre de maille sur un côté du TMesh
  MaxMerMesh = 75;   // Nombre de maille sur un côté du pMer
  SizeMap = 256;     // Taille du côté du TMesh
  sizeHauteur = 30;  // Taille hauteur du TMesh
  TailleJoueur = 2;// Taille du joueur
  MaxMeshPlus1 = MaxSolMesh+1;

var
  fPrincipale: TfPrincipale;

implementation

{$R *.fmx}

procedure TfPrincipale.ChargerTextures; // Chargement des textures
var
  Stream: TResourceStream;
begin
  maHeightMap:=TBitmap.Create;
  Stream := TResourceStream.Create(HInstance, 'heightmap', RT_RCDATA);
  maHeightMap.LoadFromStream(Stream);
  Stream.Free;
end;

procedure TfPrincipale.CreerIle(const nbSubdivisions: integer); // Création du niveau
var
  Basic : TPlane;             // TPlane qui va servir de base
  SubMap : TBitMap;           // Bitmap qui va servir pour générer le relief à partir du heightmap
  Front, Back : PPoint3D;
  M: TMeshData;               // informations du Mesh
  G, S, W, X, Y: Integer;
  hauteurMin, zMap : Single;
  C : TAlphaColorRec;         // Couleur lue dans la heightmap et qui sert à déterminer la hauteur d'un sommet
  bitmapData: TBitmapData;    // nécessaire pour pouvoir accéder aux pixels d'un TBitmap
begin
  if nbSubdivisions < 1 then exit;  // il faut au moins une subdivision

  G:=nbSubdivisions + 1;
  S:= G * G;  // Nombre total de maille
  hauteurMin := 0;

  try
    Basic := TPlane.Create(nil);    // Création du TPlane qui va servir de base à la constitution du mesh
    Basic.SubdivisionsHeight := nbSubdivisions; // le TPlane sera carré et subdivisé pour le maillage (mesh)
    Basic.SubdivisionsWidth := nbSubdivisions;

    M:=TMeshData.create;       // Création du TMesh
    M.Assign(TMEshHelper(Basic).Data); // les données sont transférées du TPlane au TMesh

    SubMap:=TBitmap.Create(maHeightMap.Width,maHeightMap.Height);  // Création du bitmap
    SubMap.Assign(maHeightMap);    // On charge la heightmap

    blur(SubMap.canvas, SubMap, 8); // On floute l'image afin d'avoir des montagnes moins anguleuses

    if (SubMap.Map(TMapAccess.Read, bitmapData)) then  // nécessaire pour accéder au pixel du Bitmap afin d'en récupérer la couleur
    begin
      try
        for W := 0 to S-1 do  // Parcours de tous les sommets du maillage
        begin
          Front := M.VertexBuffer.VerticesPtr[W];    // Récupération des coordonnées du sommet (TPlane subdivisé pour rappel : on a les coordonnées en X et Y et Z est encore à 0 pour l'instant)
          Back := M.VertexBuffer.VerticesPtr[W+S];   // Pareil pour la face arrière
          X := W mod G; // absisse du maillage en cours de traitement
          Y:=W div G; // ordonnée du maillage en cours de traitement

          C:=TAlphaColorRec(CorrectColor(bitmapData.GetPixel(x,y))); // On récupère la couleur du pixel correspondant dans la heightmap
          zMap := (C.R  + C.G  + C.B ) / $FF * sizemap / 25; // détermination de la hauteur du sommet en fonction de la couleur

          if -zMap < hauteurMin then hauteurMin := -zmap;

          Front^.Z := zMap; // on affecte la hauteur calculée à la face avant
          Back^.Z := zMap;  // pareil pour la face arrière
        end;

        M.CalcTangentBinormals; // Calcul de vecteurs binormaux et de tangente pour toutes les faces (permet par exemple de mieux réagir à la lumière)
        mSol.SetSize(sizemap, sizemap, sizeHauteur);  // Préparation du TMesh
        mSol.Data.Assign(M);  // On affecte les données du meshdata précédemment calculées au composant TMesh
      finally
        SubMap.Unmap(bitmapData);  // On libère le bitmap
      end;
    end;

    moitieCarte := math.Floor(SizeMap/2);
    demiHauteurJoueur := dmyJoueurOrientation.Height/2;
    miseAEchelle := sizeHauteur / (-hauteurMin);
    demiHauteurSol := mSol.Depth/2;

  finally
    FreeAndNil(SubMap);
    FreeAndNil(M);
    FreeAndNil(Basic);
  end;
end;

procedure TfPrincipale.faniPrincipaleProcess(Sender: TObject);
begin
   dmyProchainePosition.Position.Point := dmyJoueurOrientation.Position.Point + direction * vitesse;

    if mSol.Data.VertexBuffer.Length > 0 then
    begin
      dmyProchainePosition.Position.Y := CalculerHauteur(dmyProchainePosition.Position.Point) - demiHauteurJoueur - TailleJoueur;
      dmyJoueurOrientation.Position.Point := dmyProchainePosition.Position.Point;
    end;
end;

procedure TfPrincipale.FormCreate(Sender: TObject);
begin
  vitesse := 0;
  pMer.SubdivisionsHeight := MaxMerMesh;
  pMer.SubdivisionsWidth := MaxMerMesh;
  ChargerTextures; // Charge les différentes textures
  CreerIle(MaxSolMesh);  // Création du niveau (heightmap, immubles, arbres et autres objets
end;

procedure TfPrincipale.FormDestroy(Sender: TObject);
begin
  FreeAndNil(maHeightMap);
end;

procedure TfPrincipale.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if key = vkEscape then vitesse := 0; // Echap permet de s'arrêter
  interactionIHM;
end;

function TfPrincipale.GetDirection: TPoint3D;
begin
  result := Point3D(1,0,1) * (Camera1.AbsolutePosition - dmyJoueurOrientation.AbsolutePosition).Normalize;  // Détermination de l'orientation
end;

procedure TfPrincipale.SetAngleDeVue(const Value: TPointF);
var
  ptA,ptD,S : TPointF; // ptA point d'arrivé, ptD point de départ, S la sensibilité
begin
  S.X := 180 / v3D.Width; // Réglage de la sensibilité pour l'orientation droite/gauche
  S.Y := 180 / v3D.Height;// Réglage de la sensibilité pour l'orientation haut/bas
  ptA := Value * S;            // Point d'arrivée adapté à la sensibilité
  ptD := posDepartCurseur * S; // Point de départ adapté à la sensibilité
  // Vue droite/gauche
  with dmyJoueurOrientation.RotationAngle do y := y + (ptA.X - ptD.X); // orientation droite/gauche (axe y) en fonction du déplacement de la souris en X
  // Vue Haut/Bas
  with dmyJoueur.RotationAngle do x:= x + (ptD.Y - ptA.Y); // de même pour l'orientation haut/bas en adaptant (rotation sur l'axe x, e fonction du d'déplacement de la souris en Y
  posDepartCurseur := Value;   // la position du curseur lorsque l'utilisateur a cliqué (l'origine de la direction), est mis à jour avec la nouvelle position du curseur : au prochain appel de OnMouseMove, la position de départ doit être la position d'arrivée du coup précédent
end;

procedure TfPrincipale.v3DMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in shift then posDepartCurseur := PointF(X,Y);
end;

procedure TfPrincipale.v3DMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in shift then angleDeVue := PointF(X,Y);
  interactionIHM;
end;

procedure TfPrincipale.v3DPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  inherited;
end;

// https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Conversion_between_barycentric_and_Cartesian_coordinates
function TfPrincipale.Barycentre(p1, p2, p3 : TPoint3D; p4 : TPointF):single;
var
  det, l1, l2, l3, d1, d2, d3,  t1,t2 : single;
begin
  d1 := (p2.z - p3.z);  // Petites optimisations pour ne faire les calculs intermédiaires qu'une seule fois à chaque itération
  d2 := (p3.x - p2.x);
  d3 := (p1.x - p3.x);
  det := 1 / ((d1 * d3) + (d2 * (p1.z - p3.z))); // Inverse, permet de remplacer les divisions gourmandes par une multiplication (ainsi, on ne fait la division qu'une fois au lieu de deux à chaque itération)
  t1 := (p4.x - p3.x);
  t2 := (p4.y - p3.z);
  l1  := (( d1 * t1) + (d2 * t2 )) * det;
  l2  := ((p3.z - p1.z) * (t1 + (d3 * t2 ))) * det;
  l3  := 1 - l1 - l2;
  result := l1 * p1.y + l2 * p2.y + l3 * p3.y;
end;

function TfPrincipale.CalculerHauteur(P: TPoint3D) : single;
var
   grilleX, grilleZ : integer;
   xCoord, zCoord, hauteurCalculee : single; // coordonnées X et Z dans le "carré"
begin
  // Détermination des indices permettant d'accéder a sommet en fonction de la position du joueur
  grilleX := Math.Floor(P.X+moitieCarte);
  grilleZ := Math.Floor(P.Z+moitieCarte);

  // Si on est en dehors du mSol, on force (arbitrairement) la hauteur à la hauteur de la mer
  if (grilleX >= MaxSolMesh) or (grilleZ >= MaxSolMesh) or (grilleX < 0) or (grilleZ < 0) then
  begin
    result := 0;
  end
  else
  begin
    xCoord := Frac(P.X); // position X dans la maille courante
    zCoord := Frac(P.Z); // position y dans la maille courante

    // On calcule la hauteur en fonction des 3 sommets du triangle dans lequel se trouve le joueur
    // On détermine dans quel triangle on est
    if xCoord <= (1 - zCoord) then
    begin
      hauteurCalculee := Barycentre(TPoint3D.Create(0,-mSol.data.VertexBuffer.Vertices[grilleX + (grilleZ * MaxMeshPlus1)].Z,0),
                                  TPoint3D.Create(1,-mSol.data.VertexBuffer.Vertices[grilleX +1+ (grilleZ * MaxMeshPlus1)].Z,0),
                                  TPoint3D.Create(0,-mSol.data.VertexBuffer.Vertices[grilleX + ((grilleZ +1)* MaxMeshPlus1)].Z,1),
                                  TPointF.Create(xCoord, zCoord));
    end
    else
    begin
      hauteurCalculee := Barycentre(TPoint3D.Create(1,-mSol.data.VertexBuffer.Vertices[grilleX +1+ (grilleZ * MaxMeshPlus1)].Z,0),
                                  TPoint3D.Create(1,-mSol.data.VertexBuffer.Vertices[grilleX +1+ ((grilleZ +1) * MaxMeshPlus1)].Z,1),
                                  TPoint3D.Create(0,-mSol.data.VertexBuffer.Vertices[grilleX + ((grilleZ +1)* MaxMeshPlus1)].Z,1),
                                  TPointF.Create(xCoord, zCoord));
    end;

    hauteurCalculee := hauteurCalculee * miseAEchelle + demiHauteurSol;  // Hauteur calculée et mise à l'échelle (size 50 dans CreerIle et prise en compte des demis hauteurs)
    result := hauteurCalculee;
  end;
end;

procedure TfPrincipale.interactionIHM;
begin
  faniPrincipale.ProcessTick(0,0);      // Permet de ne pas bloquer les animations pendant que l'utilisateur interagit avec l'interface graphique
end;

procedure TfPrincipale.mSolRender(Sender: TObject; Context: TContext3D);
begin
//  Context.DrawLines(TMEshHelper(mSol).Data.VertexBuffer, TMEshHelper(mSol).Data.IndexBuffer, TMaterialSource.ValidMaterial(ColorMaterialSource2),1);
end;

procedure TfPrincipale.pMerRender(Sender: TObject; Context: TContext3D);
begin
  TTask.Create( procedure
                begin
                  CalcMesh(pMer, Point3D(0,0,pMer.Position.z), Point3D(MaxMerMesh, MaxMerMesh, 0) * 0.5 + Point3D(0,0,pMer.Position.z) * center, Point3D(0.003, 0.1, 5), MaxMerMesh);  // Animation de la mer
                end).start;
end;

procedure TfPrincipale.TrackBar1Change(Sender: TObject);
begin
  vitesse := TrackBar1.Value;
end;

// Exemple trouvé : http://edn.embarcadero.com/article/42012
procedure TfPrincipale.CalcMesh(aPlane : TPlane; origine, P, W : TPoint3D; maxMesh : integer);
var
  M:TMeshData;
  i,x,y,MaxMerMeshPlus1, lgMoins1 : integer;
  somme: single;  // Permet de cumuler les hauteurs calculer en cas de plusieurs ondes
  front, back : PPoint3D;
  F : array of TWaveRec;  // Tableau d'ondes
begin
  M:=TMeshHelper(aPlane).Data; // affectation du aPlane au TMeshData afin de pouvoir travailler avec ses mailles

  MaxMerMeshPlus1 := MaxMesh + 1;
  System.setLength(F,1);  // Nous n'utiliserons qu'une seule onde mais le code permet d'en gérer plusieurs...
  F[System.Length(F)-1].origine := origine;
  F[System.Length(F)-1].p := P;
  F[System.Length(F)-1].w := W;
  lgMoins1 := system.Length(F)-1;

  for y := 0 to MaxMesh do  // Parcours toutes les "lignes" du maillage
     for x := 0 to MaxMesh do // Parcours toutes les "colonnes" du maillage
       begin
         front := M.VertexBuffer.VerticesPtr[X + (Y * MaxMerMeshPlus1)];
         back := M.VertexBuffer.VerticesPtr[MaxMerMeshPlus1 * MaxMerMeshPlus1 + X + (Y * MaxMerMeshPlus1)];
         somme := 0; // initialisation de la somme
         for i := 0 to lgMoins1 do somme:=F[i].Wave(somme, x, y,temps); // Calcul de la hauteur du sommet de la maille
         somme := somme * 100;
         Front^.Z := somme;
         Back^.z := somme;
       end;
  M.CalcTangentBinormals;
  temps := temps + 0.005; // Incrémentation arbitraire du temps
end;

// Formule et explications : http://edn.embarcadero.com/article/42012
function TWaveRec.Wave(aSum, aX, aY, aT: single): Single;
var l : single;
begin
  l := P.Distance(Point3d(aX,aY,0));
  Result:=aSum;
  if w.Y > 0  then Result:=Result +w.x * sin (1/w.y*l-w.z*at);
end;

end.
