unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Math.Vectors, FMX.Controls3D, FMX.Objects3D, FMX.Viewport3D,
  FMX.types3D, FMX.Effects, System.UIConsts, FMX.MaterialSources, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Ani;

type
  TMeshHelper = class(TCustomMesh); // Permet d'accéder à la propriété Data du TMesh
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    mSol: TMesh;
    ColorMaterialSource2: TColorMaterialSource;
    ColorMaterialSource1: TColorMaterialSource;
    dmyMonde: TDummy;
    Layout1: TLayout;
    TrackBar1: TTrackBar;
    FloatAnimation1: TFloatAnimation;
    dmyJoueurOrientation: TDummy;
    dmyJoueur: TDummy;
    Camera1: TCamera;
    procedure FormCreate(Sender: TObject);
    procedure mSolRender(Sender: TObject; Context: TContext3D);
    procedure TrackBar1Change(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormDestroy(Sender: TObject);
  private
    FPosDepartCurseur: TPointF;    // Position du pointeur de souris au début du mouvement de la souris
    procedure SetAngleDeVue(const Value: TPointF); // Modification de l'angle de vue
    function GetDirection: TPoint3D;
    procedure ChargerTextures;
    procedure CreerIle(const nbSubdivisions: integer);
    procedure interactionIHM;
    { Déclarations privées }
  public
    { Déclarations publiques }
    maHeightMap: TBitmap;
    vitesse : single;
    property posDepartCurseur: TPointF read FPosDepartCurseur write FPosDepartCurseur; // Propriété de la position du pointeur de souris au début du mouvement de la souris
    property angleDeVue : TPointF write SetAngleDeVue; // Propriété de l'angle de vue
    property direction : TPoint3D read GetDirection; // Propriété de la direction
  end;

const
  SizeMap = 256;
  MaxSolMesh = 255;  /// Nombre de maille sur un côté du TMesh
  sizeHauteur = 30;  // Taille hauteur du TMesh

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FloatAnimation1Process(Sender: TObject);
begin
   dmyJoueurOrientation.Position.Point := dmyJoueurOrientation.Position.Point + direction * vitesse;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  trackBar1.Value := 0;
  ChargerTextures;
  CreerIle(MaxSolMesh);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(maHeightMap);
end;

procedure TForm1.mSolRender(Sender: TObject; Context: TContext3D); // Tracer les lignes des polygones
begin
  Context.DrawLines(TMEshHelper(mSol).Data.VertexBuffer, TMEshHelper(mSol).Data.IndexBuffer, TMaterialSource.ValidMaterial(ColorMaterialSource2),1);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  vitesse := TrackBar1.Value;
end;

procedure TForm1.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in shift then posDepartCurseur := PointF(X,Y);
end;

procedure TForm1.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in shift then angleDeVue := PointF(X,Y);
  interactionIHM;
end;

procedure TForm1.interactionIHM;
begin
  FloatAnimation1.ProcessTick(0,0);      // Permet de ne pas bloquer les animations pendant que l'utilisateur interagit avec l'interface graphique
end;

procedure TForm1.ChargerTextures; // Chargement des textures
var
  Stream: TResourceStream;
begin
  maHeightMap:=TBitmap.Create;
  Stream := TResourceStream.Create(HInstance, 'heightmap', RT_RCDATA);
  maHeightMap.LoadFromStream(Stream);
  Stream.Free;
end;

procedure TForm1.CreerIle(const nbSubdivisions: integer); // Création du niveau
var
  Basic : TPlane;             // TPlane qui va servir de base
  SubMap : TBitMap;           // Bitmap qui va servir pour générer le relief à partir du heightmap
  Front, Back : PPoint3D;
  M: TMeshData;               // informations du Mesh
  G, S, W, X, Y: Integer;
  zMap : Single;
  C : TAlphaColorRec;         // Couleur lue dans la heightmap et qui sert à déterminer la hauteur d'un sommet
  bitmapData: TBitmapData;    // nécessaire pour pouvoir accéder aux pixels d'un TBitmap
begin
  if nbSubdivisions < 1 then exit;  // il faut au moins une subdivision

  G:=nbSubdivisions + 1;
  S:= G * G;  // Nombre total de maille

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

  finally
    FreeAndNil(SubMap);
    FreeAndNil(M);
    FreeAndNil(Basic);
  end;
end;

procedure TForm1.SetAngleDeVue(const Value: TPointF);
var
  ptA,ptD,S : TPointF; // ptA point d'arrivé, ptD point de départ, S la sensibilité
begin
  S.X := 180 / viewport3D1.Width; // Réglage de la sensibilité pour l'orientation droite/gauche
  S.Y := 180 / viewport3D1.Height;// Réglage de la sensibilité pour l'orientation haut/bas
  ptA := Value * S;            // Point d'arrivée adapté à la sensibilité
  ptD := posDepartCurseur * S; // Point de départ adapté à la sensibilité
  // Vue droite/gauche
  with dmyJoueurOrientation.RotationAngle do y := y + (ptA.X - ptD.X); // orientation droite/gauche (axe y) en fonction du déplacement de la souris en X
  // Vue Haut/Bas
  with dmyJoueur.RotationAngle do x:= x + (ptD.Y - ptA.Y); // de même pour l'orientation haut/bas en adaptant (rotation sur l'axe x, e fonction du d'déplacement de la souris en Y
  posDepartCurseur := Value;   // la position du curseur lorsque l'utilisateur a cliqué (l'origine de la direction), est mis à jour avec la nouvelle position du curseur : au prochain appel de OnMouseMove, la position de départ doit être la position d'arrivée du coup précédent
end;

function TForm1.GetDirection: TPoint3D;
begin
  result := Point3D(1,0,1) * (Camera1.AbsolutePosition - dmyJoueurOrientation.AbsolutePosition).Normalize;  // Détermination de l'orientation
end;

end.
