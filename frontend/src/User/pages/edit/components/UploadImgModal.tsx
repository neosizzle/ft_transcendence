import React, { FunctionComponent, useCallback, useState } from "react";
import { useDropzone } from "react-dropzone";
import { API_ROOT } from "../../../../constants";
import { auth_net_patch, auth_net_post } from "../../../../utils";

interface UploadImgModalProps {
  setOpenUploadModal: React.Dispatch<React.SetStateAction<boolean>>;
  setCurrAvatar: React.Dispatch<React.SetStateAction<string>>;
}

interface UploadLabelPreviewProps {
  src: string;
  loading?: boolean;
}

interface UploadLabelProps {
  loading?: boolean;
}

interface DragNDropProps {
  setCurrFileName: React.Dispatch<React.SetStateAction<string | null>>;
  setCurrFileSrc: React.Dispatch<React.SetStateAction<string | null>>;
  currFileSrc: string | null;
}

const bucketEndpoint = `${API_ROOT}/bucket`;
const userEndpoint = `${API_ROOT}/users/me`;

const Spinner = () => {
  return (
    <div role="status">
      <svg
        aria-hidden="true"
        className="mr-2 w-8 h-8 text-gray-200 animate-spin dark:text-gray-600 fill-gray-600"
        viewBox="0 0 100 101"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
      >
        <path
          d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z"
          fill="currentColor"
        />
        <path
          d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z"
          fill="currentFill"
        />
      </svg>
      <span className="sr-only">Loading...</span>
    </div>
  );
};

const UploadLabelPreview: FunctionComponent<UploadLabelPreviewProps> = ({
  src,
  loading,
}) => {
  return (
    <div className="py-12 space-x-2">
      {loading ? (
        <Spinner />
      ) : (
        <div className="flex justify-center items-center flex-col">
          <img
            className="
					h-28
					w-28
					rounded-full
					ring-1
					ring-gray
					bg-white
					hover:h-22
					hover:w-22
					sm:hover:h-32
					sm:hover:w-32
					transition-all
					ease-out
					duration-300
					mt-3
					mb-4
					"
            src={src}
            alt=""
          />
          <div className="mb-4 w-full border-t border-2 border-gray-300"></div>
          <div className="mb-4 text-xl">
            Not happy? drag and drop to upload again
          </div>
        </div>
      )}
    </div>
  );
};

const UploadLabel: FunctionComponent<UploadLabelProps> = ({ loading }) => {
  return (
    <span className="py-12">
      {loading ? (
        <Spinner />
      ) : (
        <div>
          <div className="w-full flex justify-center my-2">
            <svg
              xmlns="http://www.w3.org/2000/svg"
              className="w-20 h-20 sm:w-26 sm:h-26 text-gray-600"
              fill="none"
              viewBox="0 0 24 24"
              stroke="currentColor"
              strokeWidth="2"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12"
              />
            </svg>
          </div>
          <div className="font-medium text-gray-600 my-2">
            Drag and Drop files to Attach
          </div>
        </div>
      )}
    </span>
  );
};

const DragNDrop: FunctionComponent<DragNDropProps> = ({
  setCurrFileName,
  setCurrFileSrc,
  currFileSrc,
}) => {
  const [loading, setLoading] = useState<boolean>(false); // loading state

  // function to run after user drops file / inputs file
  const onDrop = useCallback(async (acceptedFiles: File[]) => {
    if (!acceptedFiles || acceptedFiles.length < 1) return;

    // set loading state to true
    setLoading(true);

    // upload file to bucket
    const data = new FormData();
    data.append("file", acceptedFiles[0]);
    const res = await auth_net_post(bucketEndpoint, data, true);

    // ERROR handling

    // change current active file name and file url after upload
    setCurrFileName(acceptedFiles[0].name);
    setCurrFileSrc(`${bucketEndpoint}/${res.file}`);

    // set loading state to false
    setLoading(false);
  }, []);
  const { getRootProps, isDragActive } = useDropzone({
    onDrop,
    accept: { "image/*": [] },
  });

  return (
    <div {...getRootProps({ className: "dropzone" })} className="w-full h-full">
      {isDragActive ? (
        <div
          className="
			py-20
			"
        >
          Drop here
        </div>
      ) : currFileSrc ? (
        <UploadLabelPreview src={currFileSrc} loading={loading} />
      ) : (
        <UploadLabel loading={loading} />
      )}

      {/* <input {...getInputProps()} accept="image/*" type="file" onChange={handleFileChange} className="hidden"/> */}
    </div>
  );
};

const UploadImgModal: FunctionComponent<UploadImgModalProps> = ({
  setOpenUploadModal,
  setCurrAvatar,
}) => {
  const [currFileName, setCurrFileName] = useState<string | null>(null); // current file name
  const [currFileSrc, setCurrFileSrc] = useState<string | null>(null); // current uploaded file src
  const [loading, setLoading] = useState<boolean>(false); //confirm loading state

  return (
    <div className="fixed top-0 left-0 right-0 z-50 h-full overflow-x-hidden overflow-y-auto md:inset-0  bg-gray-300/50">
      <div className="relative w-full p-4 flex justify-center item-center">
        <div className="w-full sm:w-3/4 relative bg-white rounded-lg shadow dark:bg-gray-700">
          {/* Close button */}
          <button
            onClick={() => setOpenUploadModal(false)}
            type="button"
            className="absolute top-3 right-2.5 text-gray-400 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center dark:hover:bg-gray-800 dark:hover:text-white"
            data-modal-toggle="popup-modal"
          >
            <svg
              className="w-5 h-5"
              fill="currentColor"
              viewBox="0 0 20 20"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                fillRule="evenodd"
                d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                clipRule="evenodd"
              ></path>
            </svg>
          </button>

          {/* content */}
          <div className="p-6 text-center">
            {/* Drag and drop box */}
            <div className="w-full mt-6">
              <label className="flex justify-center w-full px-4 transition bg-white border-2 border-gray-300 border-dashed rounded-md appearance-none cursor-pointer hover:border-gray-400 focus:outline-none">
                <DragNDrop
                  setCurrFileName={setCurrFileName}
                  setCurrFileSrc={setCurrFileSrc}
                  currFileSrc={currFileSrc}
                />
              </label>

              {/* Filename */}
              <div className="flex justify-center w-full">
                <div className="md:w-2/3">
                  <input
                    disabled
                    className="bg-gray-200 appearance-none border-2 border-gray-200 rounded w-full my-4 py-2 px-4 text-gray-700 leading-tight"
                    type="text"
                    placeholder={currFileName || "No file uploaded"}
                  />
                </div>
              </div>
            </div>

            {/* Text and buttons */}
            <button
              onClick={async () => {
                if (loading) return;
                if (currFileName && currFileSrc) {
                  setLoading(true);
                  // patch request to update avatar
                  await auth_net_patch(userEndpoint, { avatar: currFileSrc });

                  //update avatar at frontend
                  setCurrAvatar(currFileSrc);
                  setOpenUploadModal(false);
                } else setOpenUploadModal(false);
              }}
              type="button"
              className={`${
                loading ? "opacity-50" : ""
              } text-white bg-green-600 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 dark:focus:ring-green-800 font-medium rounded-lg text-sm inline-flex items-center px-5 py-2.5 text-center mr-2`}
            >
              {loading ? "Loading.." : "Confirm"}
            </button>
            <button
              onClick={() => {
                if (loading) return;
                setOpenUploadModal(false);
              }}
              type="button"
              className={`${
                loading ? "opacity-50" : ""
              } text-gray-500 bg-white hover:bg-gray-100 focus:ring-4 focus:outline-none focus:ring-gray-200 rounded-lg border border-gray-200 text-sm font-medium px-5 py-2.5 hover:text-gray-900 focus:z-10 dark:bg-gray-700 dark:text-gray-300 dark:border-gray-500 dark:hover:text-white dark:hover:bg-gray-600 dark:focus:ring-gray-600`}
            >
              Cancel
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default UploadImgModal;
