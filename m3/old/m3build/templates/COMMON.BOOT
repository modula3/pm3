% Copyright (C) 1994 Digital Equipment Corporation 
% All rights reserved.
% See the file COPYRIGHT for a full description.
% 
% Last Modified On Thu Feb  9 09:14:03 PST 1995 By kalsow  

%---------------------------------------------------------------- bootstrap ---
% These functions are called to assemble the bootstrap programs
% on Unix platforms.
%

foreach f in [ "CC", "ASM", "MAKELIB", "RANLIB", "LINK" ]
  if not defined(f)
    error(format ("required parameter \"%s\" not defined%s", f, CR))
  end
end

if not defined("BOOT_CC")      BOOT_CC      = CC      end
if not defined("BOOT_ASM")     BOOT_ASM     = ASM     end
if not defined("BOOT_MAKELIB") BOOT_MAKELIB = MAKELIB end
if not defined("BOOT_RANLIB")  BOOT_RANLIB  = RANLIB  end
if not defined("BOOT_LINK")    BOOT_LINK    = LINK    end

BOOT_OBJS = []
BOOT_LIBS = []

readonly proc boot_import (file) is
  BOOT_LIBS += file
end

readonly proc boot_obj (file) is
  BOOT_OBJS += file & ".o"
end

readonly proc boot_c (file) is
  local c_file = file & ".c"
  local o_file = file & ".o"
  if stale (o_file, c_file)  exec (BOOT_CC, "-c", c_file)  end
  BOOT_OBJS += o_file
end

readonly proc _boot_asm (src, obj) is
  if stale (obj, src) exec (BOOT_ASM, "-o", obj, src) end
  BOOT_OBJS += obj
end

readonly proc boot_s  (file) is  _boot_asm (file & ".s",  file & ".o")  end
readonly proc boot_is (file) is  _boot_asm (file & ".is", file & "_i.o") end
readonly proc boot_ms (file) is  _boot_asm (file & ".ms", file & "_m.o") end

readonly proc boot_lib (file) is
  local lib = format ("lib%s.a", file)
  exec (BOOT_MAKELIB, lib, BOOT_OBJS)
  exec (BOOT_RANLIB, lib)
end

readonly proc boot_prog (file) is
  exec (BOOT_LINK, "-o", file, BOOT_OBJS, BOOT_LIBS)
end


