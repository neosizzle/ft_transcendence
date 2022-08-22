# #base image
# FROM node

# RUN mkdir /install

# #install dependencies global
# RUN npm install -g react-scripts

# ADD . /ft_transcendence-front
# WORKDIR /ft_transcendence-front

# #expose port
# EXPOSE 3000

# #start app
# CMD ["bash", "prod.sh"]

#base image
FROM node

# Create app directory
WORKDIR /ft_transcendence-front

# Install app dependencies
# A wildcard is used to ensure both package.json AND package-lock.json are copied
# where available (npm@5+)
COPY package*.json ./

RUN npm install --legacy-peer-deps
# If you are building your code for production
# RUN npm ci --only=production

# Bundle app source
COPY . .

EXPOSE 3000
CMD [ "bash", "prod.sh" ]