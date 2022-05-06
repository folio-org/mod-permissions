DO $$
BEGIN
  BEGIN
    ALTER TABLE ${myuniversity}_${mymodule}.permissions_users
        ADD CONSTRAINT permissions_users_userid_not_null CHECK (jsonb->>'userId' IS NOT NULL);
  EXCEPTION
    WHEN duplicate_object THEN
      -- nothing to do
    WHEN check_violation THEN
      DELETE FROM ${myuniversity}_${mymodule}.permissions_users WHERE jsonb->>'userId' IS NULL;
      ALTER TABLE ${myuniversity}_${mymodule}.permissions_users
          ADD CONSTRAINT permissions_users_userid_not_null CHECK (jsonb->>'userId' IS NOT NULL);
  END;
END;
$$;
