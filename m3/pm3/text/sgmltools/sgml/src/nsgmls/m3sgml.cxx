/*
  The "generic" interface of the nsgmls C++ library is used.

  All the communication between the C++ and Modula-3 modules must go through
  C structures and functions using the C calling convention. Thus, any
  call from M3 to C++ must be wrapped into a C call. C++ Method calls are
  replaced by equivalent C calls with the receiving object received as one
  of the arguments; the C call may then call the needed method.

  Calls from C++ to Modula-3 should ideally be performed by defining the
  Modula-3 procedures in an extern "Modula-3" section. This way, the
  proper internal name for the Modula-3 procedure would be used
  (e.g. ModuleName__ProcedureName on many platforms). However, this is
  not yet implemented in most/all C++ compilers. Therefore, the most portable
  way of calling M3 procedures from C/C++ is to call a C procedure from 
  Modula-3, and pass a pointer to the Modula-3 procedures to be called.
  Once the Modula-3 procedures have been stored in C pointers in this way,
  they may be called later through these procedure pointers. The Modula-3 
  procedures use the C calling convention.

  Any C++ object must either be seen from the M3 side as an opaque untraced
  reference, or be copied to an ordinary C structure. Any M3 object
  must either be seen from the C++ side as an opaque pointer, or be
  copied to a M3 RECORD. Opaque pointers to traced Modula-3 objects may
  be moved by the garbage collector unless they are referenced from the
  stack; thus a pointer to a traced Modula-3 object cannot be stored
  in a C structure, unless there is a reference to it on the stack at any time.
  Modula-3 RECORD and C structures which contain the same fields should 
  be interchangeable.
*/

#include "EventGenerator.h"
#include "ParserEventGeneratorKit.h"
#include "SGMLApplication.h"

class SGMLApplicationToM3;

/* Use the C calling convention for all structures and procedures interfacing
   to the Modula-3 world. */

extern "C" {

/* Define the procedure type and a procedure pointer for each Modula-3 
   procedure to be called from C.

   The following procedures are the callbacks from the SGML parser to the
   user defined Modula-3 application object.
*/

typedef void (*AppInfoFP)(void *object, const struct SGMLApplication::AppinfoEvent *e);
static AppInfoFP appInfoFP;

typedef void (*StartDtdFP)(void *object, const struct SGMLApplication::StartDtdEvent *e);
static StartDtdFP startDtdFP;

typedef void (*EndDtdFP)(void *object, const struct SGMLApplication::EndDtdEvent *e);
static EndDtdFP endDtdFP;

typedef void (*EndPrologFP)(void *object, const struct SGMLApplication::EndPrologEvent *e);
static EndPrologFP endPrologFP;

typedef void (*StartElementFP)(void *object, const struct SGMLApplication::StartElementEvent *e);
static StartElementFP startElementFP;

typedef void (*EndElementFP)(void *object, const struct SGMLApplication::EndElementEvent *e);
static EndElementFP endElementFP;

typedef void (*DataFP)(void *object, const struct SGMLApplication::DataEvent *e);
static DataFP dataFP;
 
typedef void (*SDataFP)(void *object, const struct SGMLApplication::SdataEvent *e);
static SDataFP sdataFP;

typedef void (*PIFP)(void *object, const struct SGMLApplication::PiEvent *e);
static PIFP piFP;

typedef void (*ExternalDataEntityRefFP)(void *object, const struct SGMLApplication::ExternalDataEntityRefEvent *e);
static ExternalDataEntityRefFP externalDataEntityRefFP;

typedef void (*SubdocEntityRefFP)(void *object, const struct SGMLApplication::SubdocEntityRefEvent *e);
static SubdocEntityRefFP subdocEntityRefFP;

typedef void (*NonSgmlCharFP)(void *object, const struct SGMLApplication::NonSgmlCharEvent *e);
static NonSgmlCharFP nonSgmlCharFP;

typedef void (*CommentDeclFP)(void *object, const struct SGMLApplication::CommentDeclEvent *e);
static CommentDeclFP commentDeclFP;

typedef void (*MarkedSectionStartFP)(void *object, const struct SGMLApplication::MarkedSectionStartEvent *e);
static MarkedSectionStartFP markedSectionStartFP;

typedef void (*MarkedSectionEndFP)(void *object, const struct SGMLApplication::MarkedSectionEndEvent *e);
static MarkedSectionEndFP markedSectionEndFP;

typedef void (*IgnoredCharsFP)(void *object, const struct SGMLApplication::IgnoredCharsEvent *e);
static IgnoredCharsFP ignoredCharsFP;

typedef void (*GeneralEntityFP)(void *object, const struct SGMLApplication::GeneralEntityEvent *e);
static GeneralEntityFP generalEntityFP;

typedef void (*ErrorFP)(void *object, const struct SGMLApplication::ErrorEvent *e);
static ErrorFP errorFP;

typedef void (*OpenEntityChangeFP)(void *object);
static OpenEntityChangeFP openEntityChangeFP;

/* These are the C procedures to be called from Modula-3. */

/* Register the pointers to all the Modula-3 procedures to be called from C */

void SetupM3SGMLProcedures(AppInfoFP fp1, StartDtdFP fp2, EndDtdFP fp3,
    EndPrologFP fp4, StartElementFP fp5, EndElementFP fp6,
    DataFP fp7, SDataFP fp8, PIFP fp9, ExternalDataEntityRefFP fp10,
    SubdocEntityRefFP fp11, NonSgmlCharFP fp12, CommentDeclFP fp13,
    MarkedSectionStartFP fp14, MarkedSectionEndFP fp15, IgnoredCharsFP fp16,
    GeneralEntityFP fp17, ErrorFP fp18, OpenEntityChangeFP fp19);

/* All the methods and creation/destruction procedures for C++ objects are
   made available as C functions to be called from Modula-3 */

/* Creation/Destruction of the C++ SGMLApplication adapter between the
   C++ parser and the Modula-3 user defined application. */

SGMLApplication *CreateM3SGMLApplication(void *peer);

void DeleteM3SGMLApplication(SGMLApplicationToM3 *sa);

/* Methods and creation/deletion procedures for the EventGenerator
   C++ object. */

void DeleteM3SGMLEventGenerator(EventGenerator *eg);

unsigned RunM3SGMLEventGenerator(EventGenerator *eg, SGMLApplicationToM3 *sa,
    void *peer);

void InhibitMessagesM3SGMLEventGenerator(EventGenerator *eg, bool b);

void HaltM3SGMLEventGenerator(EventGenerator *eg);

EventGenerator *MakeSubDocM3SGMLEventGenerator(EventGenerator *eg,
    SGMLApplication::Char *systemId, size_t systemIdLength);

/* Methods and creation/deletion procedures for the EventGeneratorKit
   C++ object. */

ParserEventGeneratorKit *CreateM3SGMLParserEventGeneratorKit();

void DeleteM3SGMLParserEventGeneratorKit(ParserEventGeneratorKit *pe);

void SetOptionM3SGML(ParserEventGeneratorKit *pe, 
    ParserEventGeneratorKit::Option option);

void SetProgramNameM3SGML(ParserEventGeneratorKit *pe, char *name);

void SetOptionWithArgM3SGML(ParserEventGeneratorKit *pe, 
    ParserEventGeneratorKit::OptionWithArg option, char *value);

EventGenerator *CreateM3SGMLEventGenerator(ParserEventGeneratorKit *pe,
    int nFiles, char **files);

/* C structure corresponding to the C++ Location object. */

struct DetailedLocation {
  unsigned long lineNumber;
  unsigned long columnNumber;
  unsigned long byteOffset;
  unsigned long entityOffset;
  SGMLApplication::CharString entityName;
  SGMLApplication::CharString filename;
};

void EntityPtrLocateM3SGMLPosition(SGMLApplicationToM3 *sa, 
    SGMLApplication::Position position, DetailedLocation *dl);
}

/* This C++ object gets called back for each significant SGML parsing
   event. It simply relays the information to its Modula-3 object
   counterpart. */

class SGMLApplicationToM3: public SGMLApplication {
public:
  void *m3peer; 
  /* Modula-3 object counterpart */

  SGMLApplication::OpenEntityPtr *currentEntity;
  /* remember the current entity, in case a detailed location is needed */

  virtual void appinfo(const AppinfoEvent &e) 
      { appInfoFP(m3peer,&e); }
  virtual void startDtd(const StartDtdEvent &e) 
      { startDtdFP(m3peer,&e); }
  virtual void endDtd(const EndDtdEvent &e) 
      { endDtdFP(m3peer,&e); }
  virtual void endProlog(const EndPrologEvent &e) 
      { endPrologFP(m3peer,&e); }
  virtual void startElement(const StartElementEvent &e) 
      { startElementFP(m3peer,&e); }
  virtual void endElement(const EndElementEvent &e) 
      { endElementFP(m3peer,&e); }
  virtual void data(const DataEvent &e) 
      { dataFP(m3peer,&e); }
  virtual void sdata(const SdataEvent &e) 
      { sdataFP(m3peer,&e); }
  virtual void pi(const PiEvent &e) 
      { piFP(m3peer,&e); }
  virtual void externalDataEntityRef(const ExternalDataEntityRefEvent &e) 
      { externalDataEntityRefFP(m3peer,&e); }
  virtual void subdocEntityRef(const SubdocEntityRefEvent &e) 
      { subdocEntityRefFP(m3peer,&e); }
  virtual void nonSgmlChar(const NonSgmlCharEvent &e) 
      { nonSgmlCharFP(m3peer,&e); }
  virtual void commentDecl(const CommentDeclEvent &e) 
      { commentDeclFP(m3peer,&e); }
  virtual void markedSectionStart(const MarkedSectionStartEvent &e) 
      { markedSectionStartFP(m3peer,&e); }
  virtual void markedSectionEnd(const MarkedSectionEndEvent &e) 
      { markedSectionEndFP(m3peer,&e); }
  virtual void ignoredChars(const IgnoredCharsEvent &e) 
      { ignoredCharsFP(m3peer,&e); }
  virtual void generalEntity(const GeneralEntityEvent &e) 
      { generalEntityFP(m3peer,&e); }
  virtual void error(const ErrorEvent &e) 
      { errorFP(m3peer,&e); }

  /* The Modula-3 world cannot do much with the OpenEntityPtr. Store the
     current entity and use it when asked for a detailed location. */

  virtual void openEntityChange(const OpenEntityPtr &e) 
      { if(currentEntity != NULL) { delete currentEntity; }
        currentEntity = new SGMLApplication::OpenEntityPtr(e);
        openEntityChangeFP(m3peer); }
};

/* Store each Modula-3 procedure pointer into a global variable. Thus,
   Modula-3 procedures can be called thereafter using these global variables. 
*/

void SetupM3SGMLProcedures(AppInfoFP fp1, StartDtdFP fp2, EndDtdFP fp3,
    EndPrologFP fp4, StartElementFP fp5, EndElementFP fp6,
    DataFP fp7, SDataFP fp8, PIFP fp9, ExternalDataEntityRefFP fp10,
    SubdocEntityRefFP fp11, NonSgmlCharFP fp12, CommentDeclFP fp13,
    MarkedSectionStartFP fp14, MarkedSectionEndFP fp15, IgnoredCharsFP fp16,
    GeneralEntityFP fp17, ErrorFP fp18, OpenEntityChangeFP fp19) {
  appInfoFP = fp1;
  startDtdFP = fp2;
  endDtdFP = fp3,
  endPrologFP = fp4;
  startElementFP = fp5;
  endElementFP = fp6,
  dataFP = fp7;
  sdataFP = fp8;
  piFP = fp9;
  externalDataEntityRefFP = fp10,
  subdocEntityRefFP = fp11;
  nonSgmlCharFP = fp12;
  commentDeclFP = fp13,
  markedSectionStartFP = fp14;
  markedSectionEndFP = fp15;
  ignoredCharsFP = fp16,
  generalEntityFP = fp17;
  errorFP = fp18;
  openEntityChangeFP = fp19;
}

/* Create and return a C++ SGMLApplication object which will be passed
   later as argument to the C++ parser object. */

SGMLApplication *CreateM3SGMLApplication(void *peer) {
  SGMLApplicationToM3 *sa = new SGMLApplicationToM3;

  /* Dont store the m3 peer yet. Not being referenced from the stack is may
     be moved by the garbage collector. */

  /* sa->m3peer = peer; */

  sa->currentEntity = NULL;
  return sa;
}

void DeleteM3SGMLApplication(SGMLApplicationToM3 *sa) {
  if(sa->currentEntity != NULL) { delete sa->currentEntity; }
  delete sa;
}

/* Relay the Modula-3 calls to EventGenerator methods. */

void DeleteM3SGMLEventGenerator(EventGenerator *eg) {
  delete eg;
}

unsigned RunM3SGMLEventGenerator(EventGenerator *eg, SGMLApplicationToM3 *sa,
    void *peer) {

  /* Now is time to store the m3 peer reference. Being referenced from
     the stack it will not be moved by the garbage collector. */

  sa->m3peer = peer;

  return eg->run(*sa);
}

void InhibitMessagesM3SGMLEventGenerator(EventGenerator *eg, bool b) {
  eg->inhibitMessages(b);
}

void HaltM3SGMLEventGenerator(EventGenerator *eg) {
  eg->halt();
}

EventGenerator *MakeSubDocM3SGMLEventGenerator(EventGenerator *eg,
    SGMLApplication::Char *systemId, size_t systemIdLength) {
  return eg->makeSubdocEventGenerator(systemId, systemIdLength);
}

/* Relay the Modula-3 calls to the EventGeneratorKit methods. */

ParserEventGeneratorKit *CreateM3SGMLParserEventGeneratorKit() {
  return new ParserEventGeneratorKit;
}

void DeleteM3SGMLParserEventGeneratorKit(ParserEventGeneratorKit *pe) {
  delete pe;
}

void SetOptionM3SGML(ParserEventGeneratorKit *pe, 
  ParserEventGeneratorKit::Option option) {
  pe->setOption(option);
}

void SetProgramNameM3SGML(ParserEventGeneratorKit *pe, char *name) {
  pe->setProgramName(name);
}

void SetOptionWithArgM3SGML(ParserEventGeneratorKit *pe, 
    ParserEventGeneratorKit::OptionWithArg option, char *value) {
  pe->setOption(option,value);
}

EventGenerator *CreateM3SGMLEventGenerator(ParserEventGeneratorKit *pe,
    int nFiles, char **files) {
  pe->makeEventGenerator(nFiles,files);
}

/* Use the stored pointer to the current entity in order to determine
   the detailed position. Extract the information from this C++ object
   and fill in the C structure received as argument. Returning a large
   C structure may cause problems as each compiler may do it differently. */

void EntityPtrLocateM3SGMLPosition(SGMLApplicationToM3 *sa, 
    SGMLApplication::Position position, DetailedLocation *dl) {
  SGMLApplication::Location location(*(sa->currentEntity),position);

  dl->lineNumber = location.lineNumber;
  dl->columnNumber = location.columnNumber;
  dl->byteOffset = location.byteOffset;
  dl->entityOffset = location.entityOffset;
  dl->entityName = location.entityName;
  dl->filename = location.filename;
}

