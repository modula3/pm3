MODULE VirtualLocalFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.6  1998/03/17 14:14:39  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.5  1998/01/21 14:12:12  roland
    New method baseName.

    Revision 1.4  1997/11/13 14:14:16  roland
    New parameter composeName for VirtualLocalFile.Open determines whether
    fileName should be treated as absolute path or as relative to its
    resource path.

    Revision 1.3  1996/11/18 17:52:19  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.2  1996/09/09 11:43:35  rbnix
    	Method getResource to relate files to their resource created
    	in base class. Therefore internal variables are removed.

    Revision 1.1  1996/02/29 17:44:20  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualLocalFile ---------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  PageFile,
  PageFileSystem,
  PageCache, SimpleMedia,
  VirtualResource, InternalVirtualResource,
  InternalVirtualFile,
  VirtualPage, VirtualLocalPage,
  ErrorSupport;


REVEAL
  T			= Public BRANDED OBJECT
      media		:SimpleMedia.T;
      fileName		:Pathname.T;
      baseName          :Pathname.T;

    OVERRIDES
      open		:= Open;
      close		:= Close;
      createPage	:= CreatePage;
      getBaseName       := GetBaseName;
    END;


PROCEDURE Open		(         self		:T;
                                  resource	:VirtualResource.T;
                                  fileName	:Pathname.T;
                                  new		:BOOLEAN;
                                  composeName   :BOOLEAN)
			:T
			RAISES {PageFile.NoAccess} =
  VAR
    file: PageFile.T;
  BEGIN
    self.baseName := fileName;
    IF composeName THEN
      (* open below client root in local resource directory *)
      self.fileName := resource.makeFileName(fileName, temporary := FALSE);
    ELSE
      self.fileName := fileName;
    END;

    TRY
      resource.registerLocalFile (self.fileName);
    EXCEPT
      VirtualResource.FatalError(info) =>
        RAISE PageFile.NoAccess(ErrorSupport.ToText(info));
    END;

    TRY
      IF new THEN
        (* make sure path exists *)
        WITH path = Pathname.Decompose(self.fileName) DO
          EVAL path.remhi();
          PageFileSystem.MakePath(Pathname.Compose(path));
        END;
      END;
      file := NEW (PageFile.T).init (self.fileName, new);
      file.open ();
    EXCEPT
    | PageFile.NoAccess (description) =>
        TRY
          resource.unregisterLocalFile (self.fileName);
        EXCEPT
          VirtualResource.FatalError => (* ignore *)
        END;
        RAISE PageFile.NoAccess (description);
    | Pathname.Invalid => RAISE PageFile.NoAccess("Local file: pathname invalid");
    END;

    self.media := NEW (SimpleMedia.T).init (file);

    RETURN self.init (resource);
  END Open;
  

PROCEDURE Close		(         self		:T)
  RAISES {VirtualResource.FatalError} =
  <* FATAL PageFile.NoAccess *>
  BEGIN
    PageCache.BeginAccess ();
    PageCache.FlushPages (self.media);
    PageCache.EndAccess ();

    self.media.getFile ().close ();
    self.getResource ().unregisterLocalFile (self.fileName);
  END Close;


PROCEDURE CreatePage	(         self		:T;
                                  pageNo	:CARDINAL)
			:VirtualPage.T =
  VAR
    page		:VirtualLocalPage.T;
  BEGIN
    PageCache.BeginAccess ();
    page := NEW (VirtualLocalPage.T).init (pageNo, self.media);
    PageCache.EndAccess ();

    RETURN page;
  END CreatePage;
    

PROCEDURE GetBaseName (self: T): Pathname.T =
  BEGIN
    RETURN self.baseName;
  END GetBaseName;
  
BEGIN
END VirtualLocalFile.
