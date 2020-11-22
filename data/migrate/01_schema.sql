--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: tbl_blog_setting; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_blog_setting (
    id bigint NOT NULL,
    ident character varying(32) NOT NULL,
    blog_name character varying(2048) NOT NULL,
    blog_url character varying(2048) NOT NULL,
    post_num smallint NOT NULL,
    media_url character varying(2048) NOT NULL,
    media_dir character varying(2048) NOT NULL,
    upload_size bigint NOT NULL,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    version bigint NOT NULL,
    adstxt character varying(512)
);


--
-- Name: tbl_blog_setting_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_blog_setting_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_blog_setting_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_blog_setting_id_seq OWNED BY tbl_blog_setting.id;


--
-- Name: tbl_comment; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_comment (
    id bigint NOT NULL,
    content character varying NOT NULL,
    status bigint NOT NULL,
    create_time timestamp with time zone,
    author character varying(512) NOT NULL,
    email character varying(512) NOT NULL,
    url character varying(4096) NOT NULL,
    post_id bigint NOT NULL
);


--
-- Name: tbl_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_comment_id_seq OWNED BY tbl_comment.id;


--
-- Name: tbl_frame; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_frame (
    id bigint NOT NULL,
    name character varying(1024),
    html character varying,
    css character varying,
    valid_flag boolean NOT NULL,
    publish_date timestamp with time zone,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    version bigint NOT NULL
);


--
-- Name: tbl_frame_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_frame_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_frame_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_frame_id_seq OWNED BY tbl_frame.id;


--
-- Name: tbl_free; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_free (
    id bigint NOT NULL,
    title character varying(2048) NOT NULL,
    content character varying NOT NULL,
    html character varying,
    css character varying,
    tags character varying(2048),
    input_type smallint NOT NULL,
    status smallint NOT NULL,
    publish_date timestamp with time zone,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    author_id bigint NOT NULL,
    version bigint NOT NULL
);


--
-- Name: tbl_free_acc; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_free_acc (
    id bigint NOT NULL,
    acc_time timestamp with time zone NOT NULL,
    view_cnt integer NOT NULL,
    free_id bigint NOT NULL
);


--
-- Name: tbl_free_acc_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_free_acc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_free_acc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_free_acc_id_seq OWNED BY tbl_free_acc.id;


--
-- Name: tbl_free_frame; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_free_frame (
    id bigint NOT NULL,
    name character varying(1024),
    html character varying,
    css character varying,
    valid_flag boolean NOT NULL,
    publish_date timestamp with time zone,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    version bigint NOT NULL
);


--
-- Name: tbl_free_frame_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_free_frame_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_free_frame_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_free_frame_id_seq OWNED BY tbl_free_frame.id;


--
-- Name: tbl_free_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_free_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_free_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_free_id_seq OWNED BY tbl_free.id;


--
-- Name: tbl_free_tag; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_free_tag (
    id bigint NOT NULL,
    free_id bigint NOT NULL,
    tag_id bigint NOT NULL
);


--
-- Name: tbl_free_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_free_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_free_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_free_tag_id_seq OWNED BY tbl_free_tag.id;


--
-- Name: tbl_home_acc; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_home_acc (
    id bigint NOT NULL,
    acc_time timestamp with time zone NOT NULL,
    view_cnt integer NOT NULL
);


--
-- Name: tbl_home_acc_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_home_acc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_home_acc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_home_acc_id_seq OWNED BY tbl_home_acc.id;


--
-- Name: tbl_media; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_media (
    id bigint NOT NULL,
    title character varying(255),
    dir character varying(2048) NOT NULL,
    file_name character varying(2048) NOT NULL,
    hash character varying(48),
    size bigint NOT NULL,
    thumb_disp_flag boolean NOT NULL,
    mime_type_id bigint,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    version bigint NOT NULL,
    author_id bigint NOT NULL
);


--
-- Name: tbl_media_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_media_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_media_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_media_id_seq OWNED BY tbl_media.id;


--
-- Name: tbl_mst_mime_type; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_mst_mime_type (
    id bigint NOT NULL,
    extension character varying(16) NOT NULL,
    mime_type character varying(128) NOT NULL,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    version bigint NOT NULL,
    delete_flag boolean NOT NULL
);


--
-- Name: tbl_mst_mime_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_mst_mime_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_mst_mime_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_mst_mime_type_id_seq OWNED BY tbl_mst_mime_type.id;


--
-- Name: tbl_mst_tag; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_mst_tag (
    id bigint NOT NULL,
    name character varying(128) NOT NULL
);


--
-- Name: tbl_mst_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_mst_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_mst_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_mst_tag_id_seq OWNED BY tbl_mst_tag.id;


--
-- Name: tbl_mst_user_perm; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_mst_user_perm (
    id bigint NOT NULL,
    level bigint NOT NULL,
    disp_order smallint NOT NULL,
    name character varying(512) NOT NULL,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    version bigint NOT NULL
);


--
-- Name: tbl_mst_user_perm_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_mst_user_perm_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_mst_user_perm_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_mst_user_perm_id_seq OWNED BY tbl_mst_user_perm.id;


--
-- Name: tbl_post; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_post (
    id bigint NOT NULL,
    title character varying(2048) NOT NULL,
    content character varying NOT NULL,
    html character varying,
    input_type smallint NOT NULL,
    tags character varying(2048),
    status smallint NOT NULL,
    publish_date timestamp with time zone,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    author_id bigint NOT NULL,
    version bigint NOT NULL
);


--
-- Name: tbl_post_acc; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_post_acc (
    id bigint NOT NULL,
    acc_time timestamp with time zone NOT NULL,
    view_cnt integer NOT NULL,
    post_id bigint NOT NULL
);


--
-- Name: tbl_post_acc_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_post_acc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_post_acc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_post_acc_id_seq OWNED BY tbl_post_acc.id;


--
-- Name: tbl_post_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_post_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_post_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_post_id_seq OWNED BY tbl_post.id;


--
-- Name: tbl_post_tag; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_post_tag (
    id bigint NOT NULL,
    post_id bigint NOT NULL,
    tag_id bigint NOT NULL
);


--
-- Name: tbl_post_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_post_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_post_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_post_tag_id_seq OWNED BY tbl_post_tag.id;


--
-- Name: tbl_user; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE tbl_user (
    id bigint NOT NULL,
    username character varying(512) NOT NULL,
    password character varying(1024),
    email character varying(512) NOT NULL,
    profile character varying(256) NOT NULL,
    create_time timestamp with time zone NOT NULL,
    update_time timestamp with time zone DEFAULT now() NOT NULL,
    version bigint NOT NULL,
    delete_flag boolean NOT NULL,
    user_perm_id bigint NOT NULL
);


--
-- Name: tbl_user_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE tbl_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: tbl_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE tbl_user_id_seq OWNED BY tbl_user.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_blog_setting ALTER COLUMN id SET DEFAULT nextval('tbl_blog_setting_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_comment ALTER COLUMN id SET DEFAULT nextval('tbl_comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_frame ALTER COLUMN id SET DEFAULT nextval('tbl_frame_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_free ALTER COLUMN id SET DEFAULT nextval('tbl_free_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_free_acc ALTER COLUMN id SET DEFAULT nextval('tbl_free_acc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_free_frame ALTER COLUMN id SET DEFAULT nextval('tbl_free_frame_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_free_tag ALTER COLUMN id SET DEFAULT nextval('tbl_free_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_home_acc ALTER COLUMN id SET DEFAULT nextval('tbl_home_acc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_media ALTER COLUMN id SET DEFAULT nextval('tbl_media_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_mst_mime_type ALTER COLUMN id SET DEFAULT nextval('tbl_mst_mime_type_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_mst_tag ALTER COLUMN id SET DEFAULT nextval('tbl_mst_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_mst_user_perm ALTER COLUMN id SET DEFAULT nextval('tbl_mst_user_perm_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_post ALTER COLUMN id SET DEFAULT nextval('tbl_post_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_post_acc ALTER COLUMN id SET DEFAULT nextval('tbl_post_acc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_post_tag ALTER COLUMN id SET DEFAULT nextval('tbl_post_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_user ALTER COLUMN id SET DEFAULT nextval('tbl_user_id_seq'::regclass);


--
-- Name: tbl_blog_setting_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_blog_setting
    ADD CONSTRAINT tbl_blog_setting_pkey PRIMARY KEY (id);


--
-- Name: tbl_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_comment
    ADD CONSTRAINT tbl_comment_pkey PRIMARY KEY (id);


--
-- Name: tbl_frame_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_frame
    ADD CONSTRAINT tbl_frame_pkey PRIMARY KEY (id);


--
-- Name: tbl_free_acc_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_free_acc
    ADD CONSTRAINT tbl_free_acc_pkey PRIMARY KEY (id);


--
-- Name: tbl_free_frame_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_free_frame
    ADD CONSTRAINT tbl_free_frame_pkey PRIMARY KEY (id);


--
-- Name: tbl_free_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_free
    ADD CONSTRAINT tbl_free_pkey PRIMARY KEY (id);


--
-- Name: tbl_free_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_free_tag
    ADD CONSTRAINT tbl_free_tag_pkey PRIMARY KEY (id);


--
-- Name: tbl_home_acc_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_home_acc
    ADD CONSTRAINT tbl_home_acc_pkey PRIMARY KEY (id);


--
-- Name: tbl_media_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_media
    ADD CONSTRAINT tbl_media_pkey PRIMARY KEY (id);


--
-- Name: tbl_mst_mime_type_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_mst_mime_type
    ADD CONSTRAINT tbl_mst_mime_type_pkey PRIMARY KEY (id);


--
-- Name: tbl_mst_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_mst_tag
    ADD CONSTRAINT tbl_mst_tag_pkey PRIMARY KEY (id);


--
-- Name: tbl_mst_user_perm_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_mst_user_perm
    ADD CONSTRAINT tbl_mst_user_perm_pkey PRIMARY KEY (id);


--
-- Name: tbl_post_acc_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_post_acc
    ADD CONSTRAINT tbl_post_acc_pkey PRIMARY KEY (id);


--
-- Name: tbl_post_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_post
    ADD CONSTRAINT tbl_post_pkey PRIMARY KEY (id);


--
-- Name: tbl_post_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_post_tag
    ADD CONSTRAINT tbl_post_tag_pkey PRIMARY KEY (id);


--
-- Name: tbl_user_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_user
    ADD CONSTRAINT tbl_user_pkey PRIMARY KEY (id);


--
-- Name: uni_free_acc_time; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_free_acc
    ADD CONSTRAINT uni_free_acc_time UNIQUE (free_id, acc_time);


--
-- Name: uni_free_tag; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_free_tag
    ADD CONSTRAINT uni_free_tag UNIQUE (free_id, tag_id);


--
-- Name: uni_home_acc_time; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_home_acc
    ADD CONSTRAINT uni_home_acc_time UNIQUE (acc_time);


--
-- Name: uni_ident; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_blog_setting
    ADD CONSTRAINT uni_ident UNIQUE (ident);


--
-- Name: uni_post_acc_time; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_post_acc
    ADD CONSTRAINT uni_post_acc_time UNIQUE (post_id, acc_time);


--
-- Name: uni_post_tag; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_post_tag
    ADD CONSTRAINT uni_post_tag UNIQUE (post_id, tag_id);


--
-- Name: uni_tag_name; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_mst_tag
    ADD CONSTRAINT uni_tag_name UNIQUE (name);


--
-- Name: uni_tbl_tbl_mst_mime_type; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_mst_mime_type
    ADD CONSTRAINT uni_tbl_tbl_mst_mime_type UNIQUE (extension);


--
-- Name: uni_tbl_user_email; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY tbl_user
    ADD CONSTRAINT uni_tbl_user_email UNIQUE (email);


--
-- Name: tbl_comment_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_comment
    ADD CONSTRAINT tbl_comment_post_id_fkey FOREIGN KEY (post_id) REFERENCES tbl_post(id);


--
-- Name: tbl_free_acc_free_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_free_acc
    ADD CONSTRAINT tbl_free_acc_free_id_fkey FOREIGN KEY (free_id) REFERENCES tbl_free(id);


--
-- Name: tbl_free_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_free
    ADD CONSTRAINT tbl_free_author_id_fkey FOREIGN KEY (author_id) REFERENCES tbl_user(id);


--
-- Name: tbl_free_tag_free_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_free_tag
    ADD CONSTRAINT tbl_free_tag_free_id_fkey FOREIGN KEY (free_id) REFERENCES tbl_free(id);


--
-- Name: tbl_free_tag_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_free_tag
    ADD CONSTRAINT tbl_free_tag_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES tbl_mst_tag(id);


--
-- Name: tbl_media_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_media
    ADD CONSTRAINT tbl_media_author_id_fkey FOREIGN KEY (author_id) REFERENCES tbl_user(id);


--
-- Name: tbl_media_mime_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_media
    ADD CONSTRAINT tbl_media_mime_type_id_fkey FOREIGN KEY (mime_type_id) REFERENCES tbl_mst_mime_type(id);


--
-- Name: tbl_post_acc_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_post_acc
    ADD CONSTRAINT tbl_post_acc_post_id_fkey FOREIGN KEY (post_id) REFERENCES tbl_post(id);


--
-- Name: tbl_post_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_post
    ADD CONSTRAINT tbl_post_author_id_fkey FOREIGN KEY (author_id) REFERENCES tbl_user(id);


--
-- Name: tbl_post_tag_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_post_tag
    ADD CONSTRAINT tbl_post_tag_post_id_fkey FOREIGN KEY (post_id) REFERENCES tbl_post(id);


--
-- Name: tbl_post_tag_tag_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_post_tag
    ADD CONSTRAINT tbl_post_tag_tag_id_fkey FOREIGN KEY (tag_id) REFERENCES tbl_mst_tag(id);


--
-- Name: tbl_user_user_perm_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY tbl_user
    ADD CONSTRAINT tbl_user_user_perm_id_fkey FOREIGN KEY (user_perm_id) REFERENCES tbl_mst_user_perm(id);


--
-- PostgreSQL database dump complete
--

