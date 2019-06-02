unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Math.Vectors, FMX.Controls3D, FMX.Objects3D, FMX.Viewport3D,
  FMX.types3D, FMX.Effects, System.UIConsts, FMX.MaterialSources, FMX.Ani;

type
  TMeshHelper = class(TCustomMesh); // Permet d'accéder à la propriété Data du TMesh
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Mesh1: TMesh;
    ColorMaterialSource2: TColorMaterialSource;
    ColorMaterialSource1: TColorMaterialSource;
    FloatAnimation1: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure Mesh1Render(Sender: TObject; Context: TContext3D);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ChargerTextures;
    procedure CreerIle(const nbSubdivisions: integer);
    { Déclarations privées }
  public
    { Déclarations publiques }
    maHeightMap: TBitmap;
  end;

const
  SizeMap = 10;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ChargerTextures;
  CreerIle(sizemap);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(maHeightMap);
end;

procedure TForm1.Mesh1Render(Sender: TObject; Context: TContext3D); // Tracer les lignes des polygones
begin
  Context.DrawLines(TMEshHelper(mesh1).Data.VertexBuffer, TMEshHelper(mesh1).Data.IndexBuffer, TMaterialSource.ValidMaterial(ColorMaterialSource2),1);
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

//    blur(SubMap.canvas, SubMap, 8); // On floute l'image afin d'avoir des montagnes moins anguleuses

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
          zMap := (C.R  + C.G  + C.B ); // détermination de la hauteur du sommet en fonction de la couleur

          Front^.Z := zMap; // on affecte la hauteur calculée à la face avant
          Back^.Z := zMap;  // pareil pour la face arrière
        end;

        M.CalcTangentBinormals; // Calcul de vecteurs binormaux et de tangente pour toutes les faces (permet par exemple de mieux réagir à la lumière)
        mesh1.SetSize(sizemap, sizemap, 1);  // Préparation du TMesh
        mesh1.Data.Assign(M);  // On affecte les données du meshdata précédemment calculées au composant TMesh
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

end.
