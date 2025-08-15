--
-- PostgreSQL database dump
--

-- Dumped from database version 17.5
-- Dumped by pg_dump version 17.5

-- Started on 2025-08-15 15:03:44

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 6 (class 2615 OID 16389)
-- Name: lntl; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA lntl;


ALTER SCHEMA lntl OWNER TO postgres;

--
-- TOC entry 4946 (class 0 OID 0)
-- Dependencies: 6
-- Name: SCHEMA lntl; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA lntl IS 'schema for the lentil project';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- TOC entry 222 (class 1259 OID 16451)
-- Name: messages; Type: TABLE; Schema: lntl; Owner: postgres
--

CREATE TABLE lntl.messages (
    id text NOT NULL,
    room_id text,
    user_id text,
    content text NOT NULL,
    "timestamp" timestamp with time zone DEFAULT now() NOT NULL,
    status text NOT NULL,
    edited_at timestamp with time zone,
    deleted_at timestamp with time zone
);


ALTER TABLE lntl.messages OWNER TO postgres;

--
-- TOC entry 221 (class 1259 OID 16432)
-- Name: room_members; Type: TABLE; Schema: lntl; Owner: postgres
--

CREATE TABLE lntl.room_members (
    room_id text NOT NULL,
    user_id text NOT NULL,
    joined_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE lntl.room_members OWNER TO postgres;

--
-- TOC entry 220 (class 1259 OID 16416)
-- Name: rooms; Type: TABLE; Schema: lntl; Owner: postgres
--

CREATE TABLE lntl.rooms (
    id text NOT NULL,
    owner_id text,
    name text NOT NULL,
    capacity integer NOT NULL,
    status text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    deleted_at timestamp with time zone
);


ALTER TABLE lntl.rooms OWNER TO postgres;

--
-- TOC entry 219 (class 1259 OID 16402)
-- Name: sessions; Type: TABLE; Schema: lntl; Owner: postgres
--

CREATE TABLE lntl.sessions (
    session_id text NOT NULL,
    user_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE lntl.sessions OWNER TO postgres;

--
-- TOC entry 218 (class 1259 OID 16390)
-- Name: users; Type: TABLE; Schema: lntl; Owner: postgres
--

CREATE TABLE lntl.users (
    id text NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    username text NOT NULL,
    dob_day integer NOT NULL,
    dob_month integer NOT NULL,
    dob_year integer NOT NULL,
    gender text NOT NULL,
    pronouns text NOT NULL,
    is_authenticated boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    password_hash text NOT NULL
);


ALTER TABLE lntl.users OWNER TO postgres;

--
-- TOC entry 4786 (class 2606 OID 16457)
-- Name: messages messages_pkey; Type: CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.messages
    ADD CONSTRAINT messages_pkey PRIMARY KEY (id);


--
-- TOC entry 4783 (class 2606 OID 16439)
-- Name: room_members room_members_pkey; Type: CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.room_members
    ADD CONSTRAINT room_members_pkey PRIMARY KEY (room_id, user_id);


--
-- TOC entry 4777 (class 2606 OID 16424)
-- Name: rooms rooms_name_key; Type: CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.rooms
    ADD CONSTRAINT rooms_name_key UNIQUE (name);


--
-- TOC entry 4780 (class 2606 OID 16422)
-- Name: rooms rooms_pkey; Type: CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.rooms
    ADD CONSTRAINT rooms_pkey PRIMARY KEY (id);


--
-- TOC entry 4773 (class 2606 OID 16409)
-- Name: sessions sessions_pkey; Type: CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (session_id);


--
-- TOC entry 4765 (class 2606 OID 16398)
-- Name: users users_pkey; Type: CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- TOC entry 4768 (class 2606 OID 16400)
-- Name: users users_username_key; Type: CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- TOC entry 4770 (class 2606 OID 16493)
-- Name: users users_username_unique; Type: CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.users
    ADD CONSTRAINT users_username_unique UNIQUE (username);


--
-- TOC entry 4774 (class 1259 OID 16499)
-- Name: idx_rooms_created_at; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX idx_rooms_created_at ON lntl.rooms USING btree (created_at DESC);


--
-- TOC entry 4787 (class 1259 OID 16470)
-- Name: messages_room_id_timestamp_idx; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX messages_room_id_timestamp_idx ON lntl.messages USING btree (room_id, "timestamp");


--
-- TOC entry 4788 (class 1259 OID 16469)
-- Name: messages_user_id_idx; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX messages_user_id_idx ON lntl.messages USING btree (user_id);


--
-- TOC entry 4784 (class 1259 OID 16450)
-- Name: room_members_user_id_idx; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX room_members_user_id_idx ON lntl.room_members USING btree (user_id);


--
-- TOC entry 4775 (class 1259 OID 16491)
-- Name: rooms_name_idx; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX rooms_name_idx ON lntl.rooms USING hash (name);


--
-- TOC entry 4778 (class 1259 OID 16430)
-- Name: rooms_owner_id_idx; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX rooms_owner_id_idx ON lntl.rooms USING btree (owner_id);


--
-- TOC entry 4781 (class 1259 OID 16431)
-- Name: rooms_status_idx; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX rooms_status_idx ON lntl.rooms USING btree (status);


--
-- TOC entry 4771 (class 1259 OID 16481)
-- Name: sessions_expires_at_idx; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX sessions_expires_at_idx ON lntl.sessions USING btree (expires_at);


--
-- TOC entry 4766 (class 1259 OID 16401)
-- Name: users_username_idx; Type: INDEX; Schema: lntl; Owner: postgres
--

CREATE INDEX users_username_idx ON lntl.users USING btree (username);


--
-- TOC entry 4790 (class 2606 OID 16494)
-- Name: rooms fk_rooms_owner; Type: FK CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.rooms
    ADD CONSTRAINT fk_rooms_owner FOREIGN KEY (owner_id) REFERENCES lntl.users(id);


--
-- TOC entry 4794 (class 2606 OID 16458)
-- Name: messages messages_room_id_fkey; Type: FK CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.messages
    ADD CONSTRAINT messages_room_id_fkey FOREIGN KEY (room_id) REFERENCES lntl.rooms(id) ON DELETE CASCADE;


--
-- TOC entry 4795 (class 2606 OID 16463)
-- Name: messages messages_user_id_fkey; Type: FK CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.messages
    ADD CONSTRAINT messages_user_id_fkey FOREIGN KEY (user_id) REFERENCES lntl.users(id) ON DELETE SET NULL;


--
-- TOC entry 4792 (class 2606 OID 16440)
-- Name: room_members room_members_room_id_fkey; Type: FK CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.room_members
    ADD CONSTRAINT room_members_room_id_fkey FOREIGN KEY (room_id) REFERENCES lntl.rooms(id) ON DELETE CASCADE;


--
-- TOC entry 4793 (class 2606 OID 16445)
-- Name: room_members room_members_user_id_fkey; Type: FK CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.room_members
    ADD CONSTRAINT room_members_user_id_fkey FOREIGN KEY (user_id) REFERENCES lntl.users(id) ON DELETE CASCADE;


--
-- TOC entry 4791 (class 2606 OID 16425)
-- Name: rooms rooms_owner_id_fkey; Type: FK CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.rooms
    ADD CONSTRAINT rooms_owner_id_fkey FOREIGN KEY (owner_id) REFERENCES lntl.users(id) ON DELETE CASCADE;


--
-- TOC entry 4789 (class 2606 OID 16410)
-- Name: sessions sessions_user_id_fkey; Type: FK CONSTRAINT; Schema: lntl; Owner: postgres
--

ALTER TABLE ONLY lntl.sessions
    ADD CONSTRAINT sessions_user_id_fkey FOREIGN KEY (user_id) REFERENCES lntl.users(id) ON DELETE CASCADE;


-- Completed on 2025-08-15 15:03:44

--
-- PostgreSQL database dump complete
--

