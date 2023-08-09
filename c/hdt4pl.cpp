/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <SWI-Stream.h>
#include <SWI-cpp2.h>
#include <iostream>
#include <HDTManager.hpp>
#include <assert.h>

#include <SWI-cpp2.cpp>

// using namespace std; TODO: remove this
using namespace hdt;

static void	deleteHDT(HDT *hdt);
static int	get_triple_role(PlTerm t, TripleComponentRole *role);

#define CATCH_HDT \
	catch (const PlExceptionBase& )		\
	{ throw;			 	\
	} catch (const std::bad_alloc& )	\
	{ throw;				\
	} catch (char *e)			\
	{ throw hdt_error(e);			\
	} catch (const char *e)			\
	{ throw hdt_error(e);			\
	} catch (std::exception& e)		\
	{ throw hdt_error(e.what());		\
	}

#define URL_xsd		  "http://www.w3.org/2001/XMLSchema#"
#define URL_xsdString     URL_xsd "string"
#define URL_xsdDouble     URL_xsd "double"

struct hdt_wrapper;

static PL_blob_t hdt_blob = PL_BLOB_DEFINITION(hdt_wrapper, "hdt");

struct hdt_wrapper : public PlBlob
{ HDT *hdt = nullptr; // TODO: can this be std::unique_ptr<HDT>?
  std::string file_name;

  explicit hdt_wrapper()
    : PlBlob(&hdt_blob)
  { }

  PL_BLOB_SIZE

  ~hdt_wrapper()
  { close();
  }

  void close() noexcept
  { if ( hdt )
    { deleteHDT(hdt);				/* FIXME: Thread safety */
      hdt = nullptr;
    }
  }

  int compare_fields(const PlBlob* _b_data) const override
  { auto b_data = static_cast<const hdt_wrapper*>(_b_data);
    return file_name.compare(b_data->file_name);
  }
};


static void
get_hdt(PlTerm t, hdt_wrapper **symb_ptr)
{ PL_blob_t *type;
  void *data;

  if ( t.get_blob(&data, NULL, &type) && type == &hdt_blob)
  { auto symb = static_cast<hdt_wrapper*>(data);

    if ( !symb->hdt )
      throw PlExistenceError("hdt", t);
    *symb_ptr = symb;
    return;
  }

  throw PlTypeError("hdt", t);
}


static void
deleteHDT(HDT *hdt)
{ delete hdt;
}

static PlException
hdt_error(const char *e)
{ throw PlGeneralError(PlCompound("hdt_error",
                                  PlTermv(PlTerm_atom(e))));
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/


PREDICATE(hdt_open_, 3)
{ static PlAtom ATOM_map("map");
  PlAtom access(ATOM_map); // default
  int indexed = true;
  PlTerm_tail options(A3);
  PlTerm_var opt;
  static PlAtom ATOM_access("access");
  static PlAtom ATOM_indexed("indexed");
  static PlAtom ATOM_load("load");

  auto symb = std::make_unique<hdt_wrapper>();

  while(options.next(opt))
  { PlAtom name(PlAtom::null);
    size_t arity;

    if ( opt.get_name_arity(&name, &arity) && arity == 1 )
    { PlTerm ov = opt[1];

      if ( name == ATOM_access )
      { ov.get_atom_ex(&access);
      } else if ( name == ATOM_indexed )
      { ov.get_bool_ex(&indexed);
      }
    } else
      throw PlDomainError("option", opt);
  }

  symb->file_name = A2.as_string();
  const char* fn = symb->file_name.c_str();

  try
  { if ( access == ATOM_map )
    { symb->hdt = indexed ?
        HDTManager::mapIndexedHDT(fn) :
        HDTManager::mapHDT(fn);
    } else if ( access == ATOM_load )
    { symb->hdt = indexed ?
        HDTManager::loadIndexedHDT(fn) :
        HDTManager::loadHDT(fn);
    } else
    { throw PlDomainError("hdt_access", PlTerm_atom(access));
    }
  } CATCH_HDT;

  std::unique_ptr<PlBlob> symb_b(symb.release());
  return A1.unify_blob(&symb_b);
}


PREDICATE(hdt_close, 1)
{ hdt_wrapper *symb;

  get_hdt(A1, &symb);
  symb->close();
  return true;
}


#define S_S 0x01
#define S_P 0x02
#define S_O 0x04

struct search_it
{ unsigned flags = 0;
  std::unique_ptr<IteratorTripleString> it;
};

static int
get_search_string(PlTerm t, char **s, unsigned flag, unsigned *flagp)
{ if ( t.is_variable() )
  { *s = (char*)"";
    *flagp |= flag;
    return true;
  } else
  { size_t len;

    return t.get_nchars(&len, s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  }
}

static int
unify_string(PlTerm t, const char *s)
{ return t.unify_chars(PL_ATOM|REP_UTF8, (size_t)-1, s);
}


static int
unify_object(PlTerm t, const char *s)
{ static PlFunctor FUNCTOR_rdftype2("^^", 2);
  static PlFunctor FUNCTOR_rdflang2("@", 2);

  if ( s[0] == '"' )
  { const char *e = s+strlen(s)-1;

    for(;; e--)
    { while( e>s && *e != '"' )
	e--;
      if ( e > s )
      { if ( e[1] == '\0' )		/* No type nor lang??  In header ... */
	{ int rc;

	  s++;
	  rc = t.unify_chars(PL_STRING|REP_UTF8, e-s, s);
	  return rc;
	} else if ( strncmp(e+1, "^^<", 3) == 0 )
	{ PlTermv av(2);
	  int rc;

	  s++;
	  rc = av[0].unify_chars(PL_STRING|REP_UTF8, e-s, s);
	  e += 4;
	  rc = rc && av[1].unify_chars(PL_ATOM|REP_UTF8, strlen(e)-1, e);
	  rc = rc && PL_cons_functor_v(av[0].C_, FUNCTOR_rdftype2.C_, av[0].C_); // TODO: av[0].cons_functor_v()
	  rc = rc && t.unify_term(av[0]);
	  return rc;
	} else if ( strncmp(e+1, "@", 1) == 0 )
	{ PlTermv av(2);
	  int rc;

	  s++;
	  rc = av[0].unify_chars(PL_STRING|REP_UTF8, e-s, s);
	  e += 2;
	  rc = rc && av[1].unify_chars(PL_ATOM|REP_UTF8, (size_t)-1, e);
	  rc = rc && PL_cons_functor_v(av[0].C_, FUNCTOR_rdflang2.C_, av[0].C_); // TODO: av[0].cons_functor_v()
	  rc = rc && t.unify_term(av[0]);
	  return rc;
	}
      } else
      { assert(0);
	return false;
      }
    }
  }

  return t.unify_chars(PL_ATOM|REP_UTF8, (size_t)-1, s);
}

/** hdt_search(+HDT, +Where, ?S, ?P, ?O)
*/

PREDICATE_NONDET(hdt_search, 5)
{ hdt_wrapper *symb;
  int rc;
  PlForeignContextPtr<search_it> ctx(handle);

  static PlAtom ATOM_content("content");
  static PlAtom ATOM_header("header");

  switch(handle.foreign_control())
  { case PL_FIRST_CALL:
    { char *s, *p, *o;
      PlAtom where(PlAtom::null);

      ctx.set(new search_it());
      get_hdt(A1, &symb);
      A2.get_atom_ex(&where);
      if ( !get_search_string(A3, &s, S_S, &ctx->flags) ||
	   !get_search_string(A4, &p, S_P, &ctx->flags) ||
	   !get_search_string(A5, &o, S_O, &ctx->flags) )
	return false;

      try
      { if ( where == ATOM_content )
          ctx->it.reset(symb->hdt->search(s,p,o));
        else if ( where == ATOM_header )
          ctx->it.reset(symb->hdt->getHeader()->search(s,p,o));
        else
          throw PlDomainError("hdt_where", A2);
      } CATCH_HDT;
    } // TODO: drop-through
    case PL_REDO:
    { if ( ctx->it->hasNext() )
      { TripleString *t = ctx->it->next();

	if ( (!(ctx->flags&S_S) || unify_string(A3, t->getSubject().c_str())) &&
	     (!(ctx->flags&S_P) || unify_string(A4, t->getPredicate().c_str())) &&
	     (!(ctx->flags&S_O) || unify_object(A5, t->getObject().c_str())) )
	{ PL_retry_address(ctx.keep());
	}
      }
      return false;
    }
    case PL_PRUNED:
      return true;
    default:
      assert(0);
      return false;
  }

  return false;
}


/** hdt_suggestions(+HDT, +From, +Role, +MaxCount, -Suggestions)
*/

PREDICATE(hdt_suggestions, 5)
{ hdt_wrapper *symb;
  TripleComponentRole role;
  char *from;
  size_t len;
  int max_count;
  std::vector<string> out;

  get_hdt(A1, &symb);
  if ( !A2.get_nchars(&len, &from,
		      CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8) ||
       !get_triple_role(A3, &role) )
    return false;
  A4.get_integer_ex(&max_count);

  try
  { symb->hdt->getDictionary()->getSuggestions(from, role, out, max_count);
  } CATCH_HDT;

  PlTerm tail = A5.copy_term_ref();
  PlTerm_var head;
  for(std::vector<string>::iterator it = out.begin();
      it != out.end();
      ++it)
  { if ( !tail.unify_list(head,tail) ||
	 !(role == OBJECT ? unify_object(head, it->c_str())
			  : unify_string(head, it->c_str())) )
      return false;
  }

  return tail.unify_nil();
}


		 /*******************************
		 *      DICTIONARY ACCESS	*
		 *******************************/

PREDICATE(hdt_property_, 2)
{ hdt_wrapper *symb;
  PlAtom name(PlAtom::null);
  size_t arity;

  get_hdt(A1, &symb);

  if ( A2.get_name_arity(&name, &arity) )
  { PlTerm a = A2[1];

    try
    { Dictionary *dict = symb->hdt->getDictionary();

      static PlAtom ATOM_mapping("mapping");
      static PlAtom ATOM_max_id("max_id");
      static PlAtom ATOM_max_object_id("max_object_id");
      static PlAtom ATOM_max_predicate_id("max_predicate_id");
      static PlAtom ATOM_max_subject_id("max_subject_id");
      static PlAtom ATOM_objects("objects");
      static PlAtom ATOM_predicates("predicates");
      static PlAtom ATOM_shared("shared");
      static PlAtom ATOM_subjects("subjects");
      static PlAtom ATOM_elements("elements");

      if ( name == ATOM_mapping )
	return a.unify_integer(dict->getMapping());
      else if ( name == ATOM_max_id )
	return a.unify_integer(dict->getMaxID());
      else if ( name == ATOM_max_object_id )
	return a.unify_integer(dict->getMaxObjectID());
      else if ( name == ATOM_max_predicate_id )
	return a.unify_integer(dict->getMaxPredicateID());
      else if ( name == ATOM_max_subject_id )
	return a.unify_integer(dict->getMaxSubjectID());
      else if ( name == ATOM_objects )
	return a .unify_integer(dict->getNobjects());
      else if ( name == ATOM_predicates )
	return a.unify_integer(dict->getNpredicates());
      else if ( name == ATOM_shared )
	return a.unify_integer(dict->getNshared());
      else if ( name == ATOM_subjects )
	return a.unify_integer(dict->getNsubjects());
      else if ( name == ATOM_elements )
	return a.unify_integer(dict->getNumberOfElements());
      else
	throw PlDomainError("hdt_property", A2);
    } CATCH_HDT;
  }

  throw PlTypeError("compound", A2);
}


struct IteratorUCharString_ctx
{ unique_ptr<IteratorUCharString> it;
};


PREDICATE_NONDET(hdt_column_, 3)
{ PlForeignContextPtr<IteratorUCharString_ctx> ctx(handle);

  switch(handle.foreign_control())
  { case PL_FIRST_CALL:
    { hdt_wrapper *symb;
      PlAtom a(PlAtom::null);
      ctx.set(new IteratorUCharString_ctx());

      get_hdt(A1, &symb);
      A2.get_atom_ex(&a);

      try
      { Dictionary *dict = symb->hdt->getDictionary();

	static PlAtom ATOM_subject("subject");
	static PlAtom ATOM_predicate("predicate");
	static PlAtom ATOM_shared("shared");
	static PlAtom ATOM_object("object");

	if ( a == ATOM_subject )
	  ctx->it.reset(dict->getSubjects());
	else if ( a == ATOM_predicate )
	  ctx->it.reset(dict->getPredicates());
	else if ( a == ATOM_shared )
	  ctx->it.reset(dict->getShared());
	else if ( a == ATOM_object )
	  ctx->it.reset(dict->getObjects());
	else
	  throw PlDomainError("hdt_column", A2);
      } CATCH_HDT;

    } // TODO: drop-through
    case PL_REDO:
      if ( ctx->it->hasNext() )
      { unsigned char *s = ctx->it->next();

	int rc = A3.unify_chars(PL_ATOM|REP_UTF8, (size_t)-1, reinterpret_cast<const char*>(s));
	ctx->it->freeStr(s);
	if ( rc )
	  PL_retry_address(ctx.keep());
      }
      return false;
    case PL_PRUNED:
      return true;
    default:
      assert(0);
      return false;
  }

  return false;
}


PREDICATE_NONDET(hdt_object_, 2)
{ PlForeignContextPtr<IteratorUCharString_ctx> ctx(handle);
  uintptr_t mask = 0;

  switch(handle.foreign_control())
  { case PL_FIRST_CALL:
    { hdt_wrapper *symb;
      ctx.set(new IteratorUCharString_ctx());

      get_hdt(A1, &symb);

      try
      { ctx->it.reset(symb->hdt->getDictionary()->getObjects());
      } CATCH_HDT;
    } // TODO: drop-through
    case PL_REDO:
      if ( ctx->it->hasNext() )
      { unsigned char *s = ctx->it->next();

	int rc = unify_object(A2, reinterpret_cast<const char*>(s));
	ctx->it->freeStr(s);
	if ( rc )
	  PL_retry_address(ctx.keep());
      }
      return false;
    case PL_PRUNED:
      return true;
  }

  return false;
}


static int
get_triple_role(PlTerm t, TripleComponentRole *role)
{ static PlAtom ATOM_subject("subject");
  static PlAtom ATOM_predicate("predicate");
  static PlAtom ATOM_object("object");

  PlAtom name(PlAtom::null);

  t.get_atom_ex(&name);
  if ( name == ATOM_subject )
    *role = SUBJECT;
  else if ( name == ATOM_predicate )
    *role = PREDICATE;
  else if ( name == ATOM_object )
    *role = OBJECT;
  else
    throw PlDomainError("hdt_role", t);

  return true;
}


/** hdt_string_id(+HDT, +Role, ?String, ?Id)
*/

PREDICATE(hdt_string_id, 4)
{ hdt_wrapper *symb;
  TripleComponentRole roleid;
  size_t len; char *s;

  get_hdt(A1, &symb);
  if ( !get_triple_role(A2, &roleid) )
    return false;

  try
  { Dictionary *dict = symb->hdt->getDictionary();

    if ( !A3.is_variable() )
    { if ( A3.get_nchars(&len, &s,
			 CVT_ATOM|CVT_STRING|REP_UTF8|CVT_EXCEPTION) )
      { std::string str(s);
	size_t id = dict->stringToId(str, roleid);

	if ( id )
	  return A4.unify_integer(id);	/* signed/unsigned mismatch */ // TODO: (long) ?
      }
    } else
    { std::string str = dict->idToString(A4.as_size_t(), roleid);

      if ( !str.empty() )
	return A3.unify_chars(PL_ATOM|REP_UTF8, (size_t)-1, str.c_str());
    }
  } CATCH_HDT;

  return false;
}


struct searchid_it
{ unsigned flags = 0;
  std::unique_ptr<IteratorTripleID> it;
};


static void
get_search_id(PlTerm t, size_t *id, unsigned flag, unsigned *flagp)
{ if ( t.is_variable() )
  { *id = 0;
    *flagp |= flag;
  } else
  { t.get_size_ex(id);
  }
}



/** hdt_search_id(+HDT, ?S, ?P, ?O)
*/

PREDICATE_NONDET(hdt_search_id, 4)
{ hdt_wrapper *symb;
  PlForeignContextPtr<searchid_it> ctx(handle);

  switch(handle.foreign_control())
  { case PL_FIRST_CALL:
    { size_t s, p, o;

      ctx.set(new searchid_it());
      get_hdt(A1, &symb);
      get_search_id(A2, &s, S_S, &ctx->flags);
      get_search_id(A3, &p, S_P, &ctx->flags);
      get_search_id(A4, &o, S_O, &ctx->flags);

      try
      { TripleID t(s,p,o);
	ctx->it.reset(symb->hdt->getTriples()->search(t));
      } CATCH_HDT;
    } // TODO: drop-through
    { if ( ctx->it->hasNext() )
      { TripleID *t = ctx->it->next();

	if ( (!(ctx->flags&S_S) || A2.unify_integer(t->getSubject())) &&
	     (!(ctx->flags&S_P) || A3.unify_integer(t->getPredicate())) &&
	     (!(ctx->flags&S_O) || A4.unify_integer(t->getObject())) )
        { PL_retry_address(ctx.keep());
	}
      }
      return false;
    }
    case PL_PRUNED:
      return true;
    default:
      assert(0);
      return false;
  }

  return false;
}


/** hdt_search_cost_id(+HDT, ?S, ?P, ?O, -Cost)
*/

PREDICATE(hdt_search_cost_id, 5)
{ hdt_wrapper *symb;
  unsigned int flags=0;
  size_t s, p, o;

  get_hdt(A1, &symb);
  get_search_id(A2, &s, S_S, &flags);
  get_search_id(A3, &p, S_P, &flags);
  get_search_id(A4, &o, S_O, &flags);

  try
  { TripleID t(s,p,o);
    unique_ptr<IteratorTripleID> it(symb->hdt->getTriples()->search(t));
    long numResults = it->estimatedNumResults();
    return A5.unify_integer(numResults);
  } CATCH_HDT;

  return true;
}


		 /*******************************
		 *	      GENERATE		*
		 *******************************/

/**
 * hdt_create_from_file(+HDTFile, +RDFFile, +Options)
 *
 * @tbd Fill HDTSpecification
 * @tbd Allow additional header triples
 */

PREDICATE(hdt_create_from_file, 3)
{ static PlAtom ATOM_base_uri("base_uri");
  char *hdt_file, *rdf_file;
  HDTSpecification spec;
  char *base_uri = (char*)"http://example.org/base";

  if ( !A1.get_file_name(&hdt_file, PL_FILE_OSPATH) ||
       !A1.get_file_name(&rdf_file, PL_FILE_OSPATH|PL_FILE_READ) )
    return false;

  PlTerm_tail options(A3);
  PlTerm_var opt;
  while(options.next(opt))
    { PlAtom name(PlAtom::null);
    size_t arity;

    if ( opt.get_name_arity(&name, &arity) && arity == 1 )
    { PlTerm ov = opt[1];

      if ( name == ATOM_base_uri )
      { size_t len;

	if ( !ov.get_nchars(&len, &base_uri,
			    CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8) )
	  return false;
      }
    } else
      throw PlTypeError("option", opt);
  }

  try
  { unique_ptr<HDT> hdt(HDTManager::generateHDT(rdf_file, base_uri, NTRIPLES, spec));

    //Header *header = hdt->getHeader();
    //header->insert("myResource1", "property", "value");

    hdt->saveToHDT(hdt_file);
  } CATCH_HDT

  return true;
}

