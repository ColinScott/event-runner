FROM localstack/localstack:latest

COPY create-resources.sh /docker-entrypoint-initaws.d/
RUN chmod 755 /docker-entrypoint-initaws.d/create-resources.sh