# #base image
# FROM node

# RUN mkdir /install

# #install dependencies
# ADD ./package.json /install
# WORKDIR /install
# # RUN apt-get update -yqq && apt-get upgrade -yqq && apt-get install postgresql-client -yqq
# RUN npm install --verbose --legacy-peer-deps
# RUN npm i -g @nestjs/cli
# ENV NODE_PATH=/install/node_modules

# ADD . /ft_transcendence-web
# WORKDIR /ft_transcendence-web

# EXPOSE 3000

# CMD ["bash", "entry.sh"]

#base image
FROM node

# Create app directory
WORKDIR /ft_transcendence-web

# Install app dependencies
# A wildcard is used to ensure both package.json AND package-lock.json are copied
# where available (npm@5+)
COPY package*.json ./

RUN npm install --legacy-peer-deps 
RUN npm install -g webpack
RUN npm link webpack
# If you are building your code for production
# RUN npm ci --only=production

# Bundle app source
COPY . .

EXPOSE 3001
CMD [ "bash", "entry.sh" ]