# Agenix secrets for weapon-server-hs
# Encrypt: agenix -e secrets/r2-credentials.age
# Rekey:   agenix -r

let
  # b7r6 user keys
  b7r6 = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ1ptqyz5C3YCcMgh3LUbXtjeS1rIZ5/6RHnH7D93Nqf"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINbn+XF6n9v9VKLFGLBVz+G1LyL6GlcgZbIwhP89PPsp"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILBEaqZY7H09brD/syW20HVDpYmKf44TOZ/Whzemwc/+"
  ];

  # shimmer host key
  shimmer = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAFyVtrt3AmJrLqcdAnZn5hXrvMenOUKGAS182qBnuYN";

  allKeys = b7r6 ++ [ shimmer ];
in
{
  "r2-credentials.age".publicKeys = allKeys;
}
