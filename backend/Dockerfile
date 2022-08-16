#base image
FROM node

RUN mkdir /install

#install dependencies
ADD ./package.json /install
WORKDIR /install
# RUN apt-get update -yqq && apt-get upgrade -yqq && apt-get install postgresql-client -yqq
RUN npm install --verbose --legacy-peer-deps
RUN npm i -g @nestjs/cli
ENV NODE_PATH=/install/node_modules

ADD . /ft_transcendence-web
WORKDIR /ft_transcendence-web

EXPOSE 3000

CMD ["bash", "entry.sh"]