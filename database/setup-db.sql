BEGIN;

CREATE TABLE article_metadata
    ( id            BIGSERIAL PRIMARY KEY
    , datex         TEXT NOT NULL
    , category      TEXT NOT NULL
    , sv_title      TEXT NOT NULL
    , en_title      TEXT NOT NULL
    , url_fragment  TEXT UNIQUE NOT NULL
    , url           TEXT UNIQUE NOT NULL
    , created_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
    , updated_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
    );

CREATE TABLE article_content
    ( url_fragment  TEXT UNIQUE NOT NULL
    , en_body       TEXT NOT NULL
    , sv_body       TEXT NOT NULL
    , created_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
    , updated_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
    );

COMMIT;
