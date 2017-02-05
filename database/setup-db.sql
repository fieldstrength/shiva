BEGIN;

    CREATE TABLE IF NOT EXISTS article_metadata
        ( date          VARCHAR NOT NULL
        , category      VARCHAR NOT NULL
        , sv_title      VARCHAR NOT NULL
        , en_title      VARCHAR NOT NULL
        , url_fragment  VARCHAR UNIQUE NOT NULL
        , url           VARCHAR UNIQUE NOT NULL
        , created_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
        , updated_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
        );

    CREATE TABLE IF NOT EXISTS article_content
        ( url_fragment  VARCHAR UNIQUE NOT NULL
        , en_body       VARCHAR NOT NULL
        , sv_body       VARCHAR NOT NULL
        , created_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
        , updated_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
        );

COMMIT;
