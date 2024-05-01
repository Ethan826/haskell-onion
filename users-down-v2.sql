DROP TRIGGER IF EXISTS trigger_check_discriminator ON organization_humans;
DROP FUNCTION IF EXISTS check_discriminator();
DROP TABLE IF EXISTS organization_humans;
DROP TABLE IF EXISTS users;
DROP TYPE IF EXISTS user_discriminator;
