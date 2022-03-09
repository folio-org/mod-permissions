DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'permissions_users_userid_not_null') THEN
    DELETE FROM ${myuniversity}_${mymodule}.permissions_users WHERE jsonb->>'userId' IS NULL;
    ALTER TABLE ${myuniversity}_${mymodule}.permissions_users
      ADD CONSTRAINT permissions_users_userid_not_null CHECK (jsonb->>'userId' IS NOT NULL);
  END IF;
END;
$$;

