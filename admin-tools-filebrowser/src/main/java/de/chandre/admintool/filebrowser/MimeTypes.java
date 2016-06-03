package de.chandre.admintool.filebrowser;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * Extensions from meta.js from CodeMirror
 * 
 * @author Andre
 * @since 1.0.1
 * 
 */
public enum MimeTypes {
	
	PLAIN("text/plain","text/plain", new HashSet<String>(Arrays.asList("txt","lxt","tex"))),
	
	XML("application/xml","application/xml", new HashSet<String>(Arrays.asList("xml","xsd","wsdl","xsl","rss"))),
	
	HTML("text/html","text/html", new HashSet<String>(Arrays.asList("htm","html","xhtml"))),
	
	CSS("text/css","text/css", new HashSet<String>(Arrays.asList("css"))),
	
	JAVASCRIPT("text/javascript", PLAIN.getMimeOrg(),  new HashSet<String>(Arrays.asList("js","javascript","ecmascript","node"))),
	
	INI("text/ini", PLAIN.getMimeOrg(), new HashSet<String>(Arrays.asList("properties","ini","in"))),
	
	JAVA(PLAIN.getMimeOrg(), PLAIN.getMimeOrg(), new HashSet<String>(Arrays.asList("java","jsp"))),
	
	PHP("application/x-httpd-php", PLAIN.getMimeOrg(), new HashSet<String>(Arrays.asList("php","php3","php4","php5","phtml"))),
	
	SHELL("text/x-sh", PLAIN.getMimeOrg(), new HashSet<String>(Arrays.asList("sh","ksh","zsh","bash","cmd","bat"))),
	
	//TODO: sort, create
	OTHER(PLAIN.getMimeOrg(), PLAIN.getMimeOrg(), new HashSet<String>(Arrays.asList(
			"dyalog","apl",
			"pgp",
			"asn","asn1",
			"b","bf",
			"c","h",
			"cpp","c++","cc","cxx","hpp","h++","hh","hxx","cpp",
			"cob","cpy",
			"cs","csharp",
			"clj","cljc","cljx",
			"cljs",
			"gss",
			"cmake","cmake.in",
			"coffee","coffee","coffee-script",
			"cl","lisp","el","lisp",
			"cyp","cypher",
			"pyx","pxd","pxi",
			"cr",
			"cql",
			"d",
			"dart",
			"diff","patch",
			"dtd",
			"dylan","dyl","intr",
			"ecl",
			"edn",
			"e",
			"elm",
			"ejs",
			"erb",
			"erl",
			"factor",
			"forth","fth","4th",
			"f","for","f77","f90",
			"fs","fsharp",
			"s",
			"feature",
			"go",
			"groovy","gradle",
			"haml",
			"hs",
			"lhs",
			"hx",
			"hxml",
			"aspx","asp","aspx",
			"pro",
			"jade",
			"json","map","json5",
			"jsonld","jsonld",
			"jsx",
			"jl",
			"kt",
			"less",
			"ls","ls",
			"lua",
			"markdown","md","mkd",
			"m","nb",
			"mo",
			"mps",
			"nsh","nsi",
			"nt",
			"m","mm",
			"ml","mli","mll","mly",
			"m",
			"oz",
			"p","pas",
			"jsonld",
			"pl","pm",
			"pig",
			"txt","text","conf","def","list","log",
			"pls",
			"proto",
			"py","pyw",
			"pp",
			"q",
			"r","rscript",
			"rst","rst",
			"spec",
			"rb","jruby","macruby","rake","rb","rbx",
			"rs",
			"sass",
			"scala",
			"scm","ss",
			"scss",
			"siv","sieve",
			"slim",
			"st",
			"tpl",
			"soy","closure template",
			"rq","sparql","sparul",
			"sql",
			"nut",
			"swift",
			"v",
			"tcl",
			"textile",
			"toml",
			"1","2","3","4","5","6","7","8","9",
			"ttcn","ttcn3","ttcnpp",
			"cfg",
			"ttl",
			"ts","ts",
			"vb",
			"vbs",
			"vtl",
			"v",
			"vhd","vhdl",
			"xy","xquery",
			"yaml","yml","yml",
			"z80",
			"mscgen","mscin","msc",
			"xu",
			"msgenny"
			
			 )))
	;
	
	private String mimeOrg;
	private String mimeGeneral;
	// use hashset for fast search
	private Set<String> extensions;
	
	private MimeTypes(String mimeOrg, String mimeGeneral, Set<String> extensions) {
		this.mimeOrg = mimeOrg;
		this.mimeGeneral = mimeGeneral;
		this.extensions = extensions;
	}

	/**
	 * @return the mimeOrg
	 */
	public String getMimeOrg() {
		return mimeOrg;
	}

	/**
	 * @return the mimeGeneral
	 */
	public String getMimeGeneral() {
		return mimeGeneral;
	}

	/**
	 * @return the extensions
	 */
	public Set<String> getExtensions() {
		return extensions;
	}

	/**
	 * returns the General Mime, for setting it to content type in http header or null
	 * @param extension
	 * @return
	 */
	public static String getMimeType(String extension) {
		for (MimeTypes types : values()) {
			if (types.extensions.contains(extension)) {
				return types.mimeGeneral;
			}
		}
		return null;
	}
}
