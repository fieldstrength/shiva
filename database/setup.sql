BEGIN;

CREATE TABLE article_metadata
    ( id            BIGSERIAL PRIMARY KEY
    , published_on  TIMESTAMP WITH TIME ZONE
    , source_name   TEXT NOT NULL
    , sv_title      TEXT NOT NULL
    , en_title      TEXT NOT NULL
    , url_fragment  TEXT UNIQUE NOT NULL
    , url           TEXT UNIQUE NOT NULL
    , created_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
    , updated_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
    );

CREATE TABLE article_content
    ( url_fragment  TEXT UNIQUE NOT NULL REFERENCES article_metadata (url_fragment)
    , content       JSONB NOT NULL
    , created_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
    , updated_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
    );

CREATE FUNCTION created_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.created_at = OLD.created_at;
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE FUNCTION updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER article_metadata_updated_at BEFORE UPDATE ON article_metadata FOR EACH ROW EXECUTE PROCEDURE updated_at();
CREATE TRIGGER article_metadata_created_at BEFORE UPDATE ON article_metadata FOR EACH ROW EXECUTE PROCEDURE created_at();

CREATE TRIGGER article_content_updated_at BEFORE UPDATE ON article_content FOR EACH ROW EXECUTE PROCEDURE updated_at();
CREATE TRIGGER article_content_created_at BEFORE UPDATE ON article_content FOR EACH ROW EXECUTE PROCEDURE created_at();

COMMIT;
